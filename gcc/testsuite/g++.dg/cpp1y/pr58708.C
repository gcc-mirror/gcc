// { dg-do run { target c++1y } }

template<typename, typename>
  struct is_same
  {
    static constexpr bool value = false;
  };

template<typename _Tp>
  struct is_same<_Tp, _Tp>
  {
    static constexpr bool value = true;
  };

template<typename CharT, CharT... Str>
  struct Foo
  {
    using char_type = CharT;
    char_type chars[sizeof...(Str)]{Str...};
  };

template<typename CharT, CharT... Str>
  Foo<CharT, Str...>
  operator""_foo()
  {
    return Foo<CharT, Str...>();
  }

int
main()
{
  auto fooU = U"\x10000\x10001\x10002"_foo;
  if (is_same<decltype(fooU)::char_type, char32_t>::value != true) __builtin_abort();
  if (sizeof(fooU.chars)/sizeof(char32_t) != 3) __builtin_abort();
  if (fooU.chars[0] != 65536) __builtin_abort();
  if (fooU.chars[1] != 65537) __builtin_abort();
  if (fooU.chars[2] != 65538) __builtin_abort();

  auto foo = "\x61\x62\x63"_foo;
  if (is_same<decltype(foo)::char_type, char>::value != true) __builtin_abort();
  if (sizeof(foo.chars)/sizeof(char) != 3) __builtin_abort();
  if (foo.chars[0] != 97) __builtin_abort();
  if (foo.chars[1] != 98) __builtin_abort();
  if (foo.chars[2] != 99) __builtin_abort();

  auto wfoo = L"\x01020304\x05060708"_foo;
  if (is_same<decltype(wfoo)::char_type, wchar_t>::value != true) __builtin_abort();
  if (sizeof(wfoo.chars)/sizeof(wchar_t) != 2) __builtin_abort();
  if (wfoo.chars[0] != 16909060) __builtin_abort();
  if (wfoo.chars[1] != 84281096) __builtin_abort();

  auto foou = u"\x0102\x0304\x0506\x0708"_foo;
  if (is_same<decltype(foou)::char_type, char16_t>::value != true) __builtin_abort();
  if (sizeof(foou.chars)/sizeof(char16_t) != 4) __builtin_abort();
  if (foou.chars[0] != 258) __builtin_abort();
  if (foou.chars[1] != 772) __builtin_abort();
  if (foou.chars[2] != 1286) __builtin_abort();
  if (foou.chars[3] != 1800) __builtin_abort();
}
