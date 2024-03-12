/* { dg-do compile } */

int
foo (int x)
{
  int *p = __builtin_alloca (x); /* { dg-error "support" } */

  return p[2];
}
