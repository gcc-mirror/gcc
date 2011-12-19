/* { dg-do compile } */
/* { dg-options "-O1" } */

typedef __complex__ double Value;

union U
{
  Value v;
  char c[sizeof(Value)];
};

struct S
{
  union U u;
  int i,j;
};

Value gv;
int gi, gj;

Value foo (void)
{
  struct S s,t;

  t.i = gi;
  t.j = gj;
  t.u.v = gv;
  t.u.c[0] = 0;

  s = t;
  __imag__ s.u.v += s.i;

  return s.u.v;
}
