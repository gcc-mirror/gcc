// { dg-final { scan-assembler "_Z1fSbB3fooIwSt11char_traitsIwESaIwEES3_" } }

namespace std {
  template <class T> struct char_traits {};
  template <class T> struct allocator {};
  template <class T, class U, class V>
  struct __attribute ((abi_tag ("foo"))) basic_string { };
  typedef basic_string<wchar_t,char_traits<wchar_t>,allocator<wchar_t> >
    wstring;
}

void f(std::wstring,std::wstring) {}
