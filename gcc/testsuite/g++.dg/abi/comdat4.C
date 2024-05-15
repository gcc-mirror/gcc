// PR lto/113208
// { dg-do compile { target { c++11 && { *-*-*gnu* } } } }
// { dg-additional-options "-O2" }
// { dg-final { scan-assembler "_ZN1BI1CEC5ERKS1_,comdat" } }
// { dg-final { scan-assembler-not "_ZN1BI1CEC1ERKS1_,comdat" } }
// { dg-final { scan-assembler-not "_ZN1BI1CEC2ERKS1_,comdat" } }

template <typename T>
struct A {
  int foo () const;
  A (int, int);
};
template <typename T>
struct B : A<T> {
  constexpr B (const B &x) : A<T> (1, x.foo ()) {}
  B () : A<T> (1, 2) {}
};
struct C;
struct D : B<C> {};
void bar (D);

void
baz (D x)
{
  bar (x);
}

template struct B<C>;
