// PR c++/120575
// { dg-additional-options -Wno-template-body }

template< int >
struct T {};

template< int >
struct S {

  operator typename T< oops >::anything () {};

};
