// PR c++/118807
// { dg-additional-options "-fmodules --param=ggc-min-expand=0 --param=ggc-min-heapsize=0 -Wno-global-module" }

module;
template <typename> class basic_streambuf;
template <typename> struct basic_streambuf {
  friend void __istream_extract();
};
template class basic_streambuf<char>;
template class basic_streambuf<wchar_t>;
export module M;
