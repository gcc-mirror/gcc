// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M:part }

module;
template <typename T> struct basic_streambuf;
template <typename T> void __copy_streambufs_eof(basic_streambuf<T>*);
template <typename T> struct basic_streambuf {
  friend void __copy_streambufs_eof<>(basic_streambuf*);
};
export module M:part;
void foo(basic_streambuf<char>&) {}
