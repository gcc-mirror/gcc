// Test that debugging backends don't crash on NULLPTR_TYPE.
// { dg-options "-std=c++0x" }

typedef decltype(nullptr) nullptr_t;

nullptr_t np1;
void f (nullptr_t) { }
template <class T> struct A { };
template <class T> nullptr_t g(T t);
template <> nullptr_t g(A<nullptr_t>)
{
  nullptr_t local;
}
// { dg-final { scan-assembler "_Z1fDn" } }
// { dg-final { scan-assembler "_Z1gI1AIDnEES1_T_" } }
