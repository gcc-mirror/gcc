// PR c++/99287
// { dg-do compile { target c++20 } }

namespace std {
struct source_location {
  static consteval source_location
  current(const void *__p = __builtin_source_location()) {
    source_location __ret;
    __ret._M_impl = static_cast<const __impl *>(__p);
    return __ret;
  }
  constexpr const char *function_name() {
    return _M_impl ? _M_impl->_M_function_name : "";
  }
  struct __impl {
    const char *_M_file_name;
    const char *_M_function_name;
    unsigned _M_line;
    unsigned _M_column;
  } const *_M_impl;
};
struct char_traits {
  static constexpr long length(const char *__s) {
    return __builtin_strlen(__s);
  }
};
template <typename _CharT, typename _Traits = char_traits>
class basic_string_view {
public:
  using traits_type = _Traits;
  using size_type = unsigned long;
  constexpr basic_string_view(const _CharT *__str)
      : _M_len{traits_type::length(__str)}, _M_str{__str} {}
  constexpr size_type find(const _CharT *, size_type, size_type) const noexcept;
  constexpr size_type find(_CharT *__str) {
    long __trans_tmp_1 = traits_type::length(__str);
    return find(__str, 0, __trans_tmp_1);
  }
  long _M_len;
  const _CharT *_M_str;
};
using string_view = basic_string_view<const char>;
template <typename _CharT, typename _Traits>
constexpr unsigned long
basic_string_view<_CharT, _Traits>::find(const _CharT *__str, size_type,
                                         size_type __n) const noexcept {
  int __trans_tmp_2;
  const _CharT *__first = _M_str;
  size_type __len = _M_len;
  while (__len >= __n) {
    __trans_tmp_2 = __builtin_memcmp(__first, __str, __n);
    if (__trans_tmp_2 == 0)
      return __first - _M_str;
    __len = _M_str - ++__first;
  }
}
} // namespace std
template <typename> consteval auto f() {
  return std::string_view{std::source_location::current().function_name()};
}
int main() { constexpr auto s = f<int>().find("int"); }
