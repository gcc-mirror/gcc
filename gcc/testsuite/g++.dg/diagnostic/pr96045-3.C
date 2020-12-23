// PR 96045  EOF location

#define BORKED <int> // { dg-error "20:" }

template <class> class A {};
struct A BORKED // { dg-message "10: in expansion of" }
