// Test that using T{} at file scope doesn't create a static temporary.
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "local" } }

struct A { };

A a = A{};
