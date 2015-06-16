// PR c++/66387
// { dg-do compile { target c++14 } }

namespace boost {
namespace hana {
namespace detail {
namespace std {
using size_t = decltype(0);
}
}
namespace ic_detail {
template <typename T, T> struct _with_index {
  template <typename F> constexpr void operator()(F &&) const;
};
template <typename T, T v> struct _times { _with_index<T, v> with_index; };
}
template <typename T, T v> struct _integral_constant {
  using value_type = T;
  operator value_type() const;
  ic_detail::_times<T, v> times;
};
template <detail::std::size_t i>
constexpr _integral_constant<detail::std::size_t, i> size_t{};
template <typename, typename = void> struct datatype;
}
}
namespace std {
typedef int size_t;
inline namespace __cxx11 {}
}
namespace boost {
namespace hana {
template <bool> struct when;
template <typename, typename, typename> struct to_impl;
template <typename T, typename> struct datatype : datatype<T, when<true>> {};
template <typename T, bool condition> struct datatype<T, when<condition>> {
  using type = typename T::hana::datatype;
};
template <typename> struct _models;
template <typename To, typename From>
    struct to_impl < To,
    From, when < _models<From> {
} >> ;
namespace detail {
namespace std {
template <typename T, T> struct integer_sequence;
template <size_t... n> using index_sequence = integer_sequence<size_t, n...>;
namespace int_seq_detail {
template <size_t> struct make_index_sequence {
  using type = index_sequence<0>;
};
template <typename, typename> struct cast_to;
template <typename T, typename U, U... u>
struct cast_to<T, integer_sequence<U, u...>> {
  using type = integer_sequence<T, u...>;
};
}
template <typename T, T>
using make_integer_sequence = typename int_seq_detail::cast_to<
    T, int_seq_detail::make_index_sequence<1>::type>::type;
}
}
namespace ic_detail {
template <typename T, T N, typename = detail::std::make_integer_sequence<T, N>>
struct go;
template <typename T, T N, T... i>
struct go<T, N, detail::std::integer_sequence<T, i...>> {
  using swallow = T;
  template <typename F> static void with_index(F f) {
    swallow{(f(_integral_constant<T, i>{}), 0)...};
  }
};
template <typename T, T v>
template <typename F>
constexpr void _with_index<T, v>::operator()(F &&f) const {
  go<T, 0>::with_index(f);
}
}
}
}
namespace std {
template <typename> class allocator;
template <class> struct char_traits;
template <typename _CharT, typename = char_traits<_CharT>> class basic_ostream;
namespace __cxx11 {
template <typename _CharT, typename = char_traits<_CharT>,
          typename = allocator<_CharT>>
class basic_stringstream;
}
typedef basic_ostream<char> ostream;
typedef basic_stringstream<char> stringstream;
template <typename, typename> class basic_ostream {};
template <typename _CharT, typename>
class basic_iostream : public basic_ostream<_CharT> {};
namespace __cxx11 {
template <typename _CharT, typename _Traits, typename>
class basic_stringstream : public basic_iostream<_CharT, _Traits> {};
}
}
namespace hana = boost::hana;
template <typename> struct print_impl;
template <typename X> void print(std::ostream os, X x) {
  using Tag = typename hana::datatype<X>::type;
  print_impl<Tag>::apply(os, x);
}
struct Vector;
template <typename, typename> struct vector2 {
  struct hana {
    using datatype = Vector;
  };
  static constexpr std::size_t size = 0;
};
template <> struct print_impl<Vector> {
  template <typename vectorN> static void apply(std::ostream, vectorN) {
    constexpr auto N = hana::size_t<vectorN::size>;
    N.times.with_index([&](auto) { N - hana::size_t<1>; });
  }
};
int main() {
  std::stringstream ss;
  vector2<int, char> v2;
  print(ss, v2);
}
