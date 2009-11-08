// PR c++/26571

struct A {};
unsigned A a;			// { dg-error "expected initializer" }
