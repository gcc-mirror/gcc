/* Copy of gcc.c-torture/compile/pr80443.c  */
/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv" } */

struct S { int a : 1; } b, c;
signed char d, e, f;

void
foo ()
{ 
  while (f)
    { 
      signed char g = b.a;
      if (g)
	b.a = ~(1 + (d || c.a));
      if (b.a < g && b.a)
	g = 0;
      if (b.a > c.a)
	b.a = g;
      c.a = e;
    }
}
