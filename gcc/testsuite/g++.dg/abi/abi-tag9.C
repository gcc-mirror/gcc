// { dg-final { scan-assembler "_Z1fSsB3fooS_" } }

namespace std {
  template <class T> struct char_traits {};
  template <class T> struct allocator {};
  template <class T, class U, class V>
  struct __attribute ((abi_tag ("foo"))) basic_string { };
  typedef basic_string<char,char_traits<char>,allocator<char> > string;
}

void f(std::string,std::string) {}
