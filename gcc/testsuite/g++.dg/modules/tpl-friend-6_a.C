// { dg-additional-options -fmodules-ts }
// befriending a specialization

export module foo;
// { dg-module-cmi foo }

namespace not_std {

template<typename T>
class basic_streambuf;

template<typename T>
void __copy_streambufs_eof(basic_streambuf<T>*);

template<typename T>
class basic_streambuf
{
  friend void __copy_streambufs_eof<>(basic_streambuf*);

  T member;
};

template<>
void
__copy_streambufs_eof(basic_streambuf<char>* __sbin);
}
