// PR c++/16370

struct Foo { int i; } __attribute__ ((deprecated));
void foo() { Foo f; }		// { dg-warning "deprecated" }
