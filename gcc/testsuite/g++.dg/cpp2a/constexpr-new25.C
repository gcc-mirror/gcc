// PR c++/118775
// { dg-do compile { target c++20 } }

namespace std {
struct __uniq_ptr_impl {
  constexpr __uniq_ptr_impl(char *) {}
};
template <typename> struct unique_ptr {
  __uniq_ptr_impl _M_t;
  constexpr ~unique_ptr() {}
};
template <typename> struct _MakeUniq;
template <typename _Tp> struct _MakeUniq<_Tp[]> {
  typedef unique_ptr<_Tp[]> __array;
};
template <typename _Tp> using __unique_ptr_array_t = _MakeUniq<_Tp>::__array;
constexpr __unique_ptr_array_t<char[]> make_unique(long __num) {
  return unique_ptr<char[]>(new char[__num]);
}
} // namespace std
int a;
int
main ()
{
  std::unique_ptr p = std::make_unique((long)&a);
  constexpr std::unique_ptr p2 = std::make_unique((long)&a); // { dg-error "conversion" }
}
