// Test that the parens don't show up in the mangling
// { dg-do compile { target c++14 } }
// { dg-options "-Wno-return-local-addr" }
// { dg-final { scan-assembler "_Z1gI1AEDTdtfp_1iET_" } }

struct A { int i; };

template <class T>
auto g(T t)->decltype((t.i)) { return t.i; }

int main()
{
  g(A());
}
