// { dg-options "-Wabi-tag" }

template<class T>
struct __attribute ((__abi_tag__("cxx11"))) list // { dg-message "list" }
{ };

struct X {			// { dg-warning "ABI tag" }
  list<int> l;			// { dg-message "X::l" }
};
