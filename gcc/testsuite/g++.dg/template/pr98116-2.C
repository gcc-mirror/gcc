// PR 98116, ICE with stripping typedef array type
// { dg-do compile { target c++11 } }
// { dg-additional-options {--param=hash-table-verification-limit=10000000 -fchecking=2} }

// We got confused by alias templates that alias the same type.  Their
// hashes were different (good), but they compared equal (bad)

namespace std {
typedef int is_convertible;
template <typename _Tp> using remove_pointer_t = typename _Tp ::type;
template <bool> struct enable_if;
template <typename> void declval();
template <bool _Cond> using enable_if_t = typename enable_if<_Cond>::type;
template <typename, typename> class Trans_NS___cxx11_basic_string {
  long _M_string_length;
};
} // namespace std
struct string16_char_traits;
template class std::Trans_NS___cxx11_basic_string<unsigned short,
                                                  string16_char_traits>;
template <typename, typename> using IsLegalDataConversion = std::is_convertible;
template <typename Container, typename T>
using ContainerHasConvertibleData = IsLegalDataConversion<
    std::remove_pointer_t<decltype(std::declval<Container>)>, T>;
template <typename Array, typename T, long>
using EnableIfSpanCompatibleArray =
  std::enable_if_t<!!sizeof (ContainerHasConvertibleData<Array, T>)>;
template <int Extent> class span {
  template <long N, EnableIfSpanCompatibleArray<
                        const std::Trans_NS___cxx11_basic_string<
                            unsigned short, string16_char_traits>[N],
                        std::Trans_NS___cxx11_basic_string<short, int>, Extent>>
  span();
};
