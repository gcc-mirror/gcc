// PR 96045  EOF location

template <class> class A {};
struct A <int> // { dg-error "15:" }
/* A comment */
  
