// Test that using T{} at file scope doesn't create a static temporary.
// { dg-options -std=c++11 }
// { dg-final { scan-assembler-not "local" } }

struct A { };

A a = A{};
