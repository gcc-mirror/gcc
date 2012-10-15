// PR debug/54831
// { dg-do compile }
// { dg-options "-O -fno-split-wide-types -g" }

struct S
{
  int m1();
  int m2();
};

typedef void (S::*mptr) ();

mptr gmp;
void bar (mptr f);

void foo (mptr f)
{
  f = gmp;
  bar (f);
}
