// PR c++/32470

// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }
// { dg-final { scan-hidden "_ZN4Test4testEv" } }

namespace std __attribute__((__visibility__("default"))) {
  template<typename _CharT>
  class basic_streambuf
  {
    friend void getline();
  };
  extern template class basic_streambuf<char>;
}

class Test
{
  void test();
};
void Test::test() { }
