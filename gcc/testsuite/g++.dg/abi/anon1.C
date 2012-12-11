// PR c++/54883

namespace { enum E { E1 }; } void f(E e) { }

// { dg-final { scan-assembler-not "globl" } }
