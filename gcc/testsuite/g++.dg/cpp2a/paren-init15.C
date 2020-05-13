// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++20 } }

struct A {
  int i;
  int j;
};

struct B : A
{
  B (): A(1.7, 2) { }
};

void f(A);

void
g ()
{
  f (A(1, 2));
}

struct S {
  int a[2];
};

S h() { return S({1, 2}); }

struct Z {
  int i;
  int j;
  operator A();
};

Z z;
A a = Z(1, 2.3);
