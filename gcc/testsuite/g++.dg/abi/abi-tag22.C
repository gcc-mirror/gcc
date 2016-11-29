// PR c++/71712
// { dg-options -Wabi=10 }

struct __attribute__((abi_tag("A", "B"))) A { };
struct A18 {
  operator A();			// { dg-warning "mangled name" }
};
void f18_test() {
  // { dg-final { scan-assembler "_ZN3A18cv1AB1AB1BEv" } }
  A a = A18();
}
