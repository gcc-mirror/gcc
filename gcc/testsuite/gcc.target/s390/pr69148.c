/* { dg-do compile } */
/* { dg-options "-O -march=z196 -m64 -w" } */
union U { int r; float f; };
struct A {
  int a;
    union U b[64];
    };
    double foo (double);

void
bar (struct A *z, int x)
{
  union U y;
  y.f = foo (z->b[x].f);
  z->a = y.r ? 4 : y.r;
}
