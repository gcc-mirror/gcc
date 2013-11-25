// { dg-do compile }

typedef int FIC(int) const;
typedef int FI(int);

FIC f; // { dg-error "cv-qualifier" }
struct S {
  FIC f; // OK

  const FI g;

  int h(int) const;

};
FIC S::*pm = &S::f;
const FI S::*pm2 = &S::f; // { dg-error "cannot convert" }
const FIC S::*pm3 = &S::f;

int S::f(int) const
{
  return 17;
}


int foo(float) const // { dg-error "qualifier" }
{
  return 0;
}

int bar(float) volatile; // { dg-error "qualifier" }
