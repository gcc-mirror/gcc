struct S {};

void f (int S::*const*);

typedef int I;

void f (I S::*const*);

void g() {
  int S::*const* p;

  f(p);
}
