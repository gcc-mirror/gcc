// PR c++/47289
// { dg-options -std=c++0x }
// { dg-prune-output "note" }

template <template <typename... __ARGS> class _F, typename... _ARGS>
auto reverse (_ARGS... args) -> decltype(_F<_ARGS...>::call_function(args...)) {
  return _F<_ARGS...>::call_function(args...);
}

template <typename _T>
_T sum(_T x) { return x; }

template <typename _T, typename... _ARGS>
_T sum(_T x, _ARGS... args) { return x + sum(args...); }

template <typename _T, typename... _ARGS>
struct call_sum {
  static _T call_function(_T x1, _ARGS... args) { return sum(x1, args...); }
};

int main() {
  // This shouldn't be an error; this is bug 35722.
  reverse<call_sum>(1,2);	// { dg-bogus "no match" "" }
  // { dg-bogus "sorry, unimplemented" "candidate explanation" { target *-*-* } 6 }
}
