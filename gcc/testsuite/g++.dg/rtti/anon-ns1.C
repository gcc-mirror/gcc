// PR c++/49440
// The typeinfo name for A should start with * so we compare
// it by address rather than contents.

// { dg-final { scan-assembler "\"\*N\[^\"\]+1AE\"" } }

namespace
{
  class A { };
}

void f()
{
  throw A();
}
