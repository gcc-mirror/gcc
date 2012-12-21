/* { dg-do compile { target powerpc-ibm-aix* } } */

extern struct { int a, b, c, d; } v;
extern int w;

void
foo (void)
{
  int e1 = v.a;
  int e2 = w;
  int e3 = v.b;
  int e4 = v.c;
  int e5 = v.d;
  __asm__ volatile ("" : : "nro" (e1), "nro" (e2), "nro" (e3), "nro" (e4), "nro" (e5));
}

