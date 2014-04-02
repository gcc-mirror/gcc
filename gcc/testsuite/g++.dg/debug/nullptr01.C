// Test that debugging backends don't crash on NULLPTR_TYPE.
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }

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
// { dg-final { scan-assembler "_Z1gI1AIDnEEDnT_" } }
