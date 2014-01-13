// { dg-options -Wabi-tag }
// { dg-final { scan-assembler "_Z1f1BI1AB3fooE" } }

struct __attribute__ ((abi_tag ("foo"))) A { };
template <class T> struct B: T { };

void f(B<A>) {}
