/* { dg-do compile } */
/* { dg-options "-Wunused -W" } */

int
f1 (unsigned int x)
{
  int c = ({ union { unsigned int a; int b; } u; u.a = x; u.b; });
  return c;
}

void
f2 (void)
{
  struct S { int i; } a;
  int b[1];
  a.i = 1;
  a.i;				/* { dg-warning "no effect" } */
  b[0] = 1;
  b[0];				/* { dg-warning "no effect" } */
}

void
f3 (void)
{
  struct S { int i; } a;	/* { dg-warning "set but not used" } */
  int b[1];			/* { dg-warning "set but not used" } */
  a.i = 1;
  b[0] = 1;
}
