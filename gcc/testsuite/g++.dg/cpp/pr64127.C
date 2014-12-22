/* { dg-do compile { target c++98_only } } */

template <0> int __copy_streambufs_eof; // { dg-error "expected identifier|numeric constant|variable templates" }
__copy_streambufs_eof < // { dg-error "template argument|parse error|not name a type" }
