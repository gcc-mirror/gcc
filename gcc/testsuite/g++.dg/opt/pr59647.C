// PR rtl-optimization/59647
// { dg-do compile }
// { dg-options "-O2 -fno-tree-vrp" }
// { dg-additional-options "-msse2 -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ia32 } } }

void f1 (int);
void f2 ();
double f3 (int);

struct A
{
  int f4 () const
  {
    if (a == 0)
      return 1;
    return 0;
  }
  unsigned f5 ()
  {
    if (!f4 ())
      f2 ();
    return a;
  }
  int a;
};

void
f6 (A *x)
{
  unsigned b = x->f5 ();
  f1 (b - 1 - f3 (x->f5 () - 1U));
}
