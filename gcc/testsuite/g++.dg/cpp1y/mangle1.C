// Test that the parens don't show up in the mangling
// { dg-options "-std=c++1y -Wno-return-local-addr" }
// { dg-final { scan-assembler "_Z1gI1AEDTdtfp_1iET_" } }

struct A { int i; };

template <class T>
auto g(T t)->decltype((t.i)) { return t.i; }

int main()
{
  g(A());
}
