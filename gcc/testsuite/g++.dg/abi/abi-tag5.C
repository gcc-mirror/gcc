// { dg-options -Wabi-tag }

struct __attribute__ ((abi_tag ("foo"))) A { };
template <class T> struct B: T { };

B<A> b;
