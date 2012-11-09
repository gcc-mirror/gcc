void f(int);
void f(int) __attribute ((abi_tag ("foo"))); // { dg-error "adds abi tag" }

struct C;
struct __attribute ((abi_tag ("foo"))) C; // { dg-error "adds abi tag" }
