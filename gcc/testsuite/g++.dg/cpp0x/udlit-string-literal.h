#pragma GCC system_header

#include <string>

inline namespace my_string_literals
{
  std::string
  operator ""s(const char* str, std::size_t len)
  { return std::string{str, len}; }

#if __cpp_lib_char8_t
  std::u8string
  operator ""s(const char8_t* str, std::size_t len)
  { return std::u8string{str, len}; }
#endif

  std::wstring
  operator ""s(const wchar_t* str, std::size_t len)
  { return std::wstring{str, len}; }

  std::u16string
  operator ""s(const char16_t* str, std::size_t len)
  { return std::u16string{str, len}; }

  std::u32string
  operator ""s(const char32_t* str, std::size_t len)
  { return std::u32string{str, len}; }
}
