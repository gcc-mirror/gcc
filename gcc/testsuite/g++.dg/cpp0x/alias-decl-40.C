// PR c++/58170
// { dg-require-effective-target c++11 }
// { dg-prune-output "not declared" }
// { dg-prune-output "expected" }

template <typename T, typename U>
struct base {
  template <typename V>
  struct derived;
};

template <typename T, typename U>
template <typename V>
struct base<T, U>::derived : public base<T, V> {
};

// This (wrong?) alias declaration provokes the crash.
template <typename T, typename U, typename V>
using alias = base<T, U>::derived<V>; // { dg-error "template|typename" }

// This one works:
// template <typename T, typename U, typename V>
// using alias = typename base<T, U>::template derived<V>;

template <typename T>
void f() {
  alias<T, bool, char> m{};
  (void) m;
}

int main() {
  f<int>();
}
