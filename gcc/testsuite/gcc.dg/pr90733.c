/* PR debug/90733 */
/* { dg-do compile } */
/* { dg-options "-g -O2 -w" } */

struct S { unsigned a : 1; };
union U { struct S b; _Complex unsigned c; };

union U
foo (union U d)
{
  union U e = d;
  return e;
}

int
bar (void)
{
  union U x, y;
  x.c = x.b.a;
  y = foo (x);
  return x.c != y.c;
}
