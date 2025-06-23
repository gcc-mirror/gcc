// { dg-options "-w" }

struct Foo(usize);

const B: usize = A.0;
const A: Foo = Foo(123);
