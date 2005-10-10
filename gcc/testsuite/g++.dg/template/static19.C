// PR c++/24275

template <bool val>  struct bool_var {
  static const bool value = val;
};
namespace is_inc_ {
  struct any {
    template <class T> any(T const&);
  };
  int operator++(any const&);
  template <class T>   struct impl {
    static T &x;
    static const bool value = sizeof(++x) == 1;
  };
}
template<typename T> struct is_incr : bool_var< is_inc_::impl<T>::value> {};
struct not_incr{};
typedef int sa1[ is_incr<not_incr>::value ? -1 : 1];
