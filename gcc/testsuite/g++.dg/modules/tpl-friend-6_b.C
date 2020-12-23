// { dg-additional-options -fmodules-ts }
module foo;

namespace not_std {

template<>
void
__copy_streambufs_eof(basic_streambuf<char>* __sbin)
{
  __sbin->member = 0;
}

}
