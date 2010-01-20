// { dg-do compile }
// { dg-options "-pedantic -pedantic-errors" }
typedef int FIC(int) const;
typedef int FI(int);

FIC f; // { dg-error "qualified" }
struct S {
  FIC f; // OK

  const FI g; // { dg-error "qualifier" }

  int h(int) const;

};
FIC S::*pm = &S::f;
const FI S::*pm2 = &S::f; // { dg-error "qualifier" }
// { dg-error "cannot convert" "cannot convert" { target *-*-* } 16 }
const FIC S::*pm3 = &S::f; // { dg-error "qualifier" }

int S::f(int) const
{
  return 17;
}


int foo(float) const // { dg-error "qualifier" }
{
  return 0;
}

int bar(float) volatile; // { dg-error "qualifier" }
