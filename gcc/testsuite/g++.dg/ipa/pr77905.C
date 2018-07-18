// PR ipa/77905
// { dg-do compile }
// { dg-options "-O2" }

struct A {
  A(int);
};
struct B : A {
  B();
} A;
struct C : virtual A {
  C(int);
};
A::A(int x) {
  if (x)
    A(0);
}

B::B() : A(1) {}

C::C(int) : A(1) {}
