// PR c++/60642

template<typename T>
class __attribute((abi_tag("foo"))) test{  };

template class __attribute((abi_tag("foo"))) test<int>; // { dg-warning "attribute" }

void f(test<char>*) {}
// { dg-final { scan-assembler "_Z1fP4testB3fooIcE" } }
