/* This testcase caused ICE because gcc was not able to add instructions
   on edge from ENTRY block successor to itself.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fssa" } */

struct A { int a1; int a2; };
struct B { long int b[32]; };

extern int bar (struct B *, struct A *);

int
foo (struct B x)
{
  struct A a, b;
  struct B c;
  int d;

  while (1)
    {
      a.a1 = 0;
      a.a2 = 0;
      b = a;
      c = x;
      d = bar (&c, &b);
      if (d >= 0)
        return d;
    }

  return 0;
}
