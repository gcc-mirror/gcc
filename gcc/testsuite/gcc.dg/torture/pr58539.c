/* { dg-do compile } */
/* { dg-options "-g" } */

int a, b;

extern void baz (int);

int foo (int p)
{
  return p ? p : 1;
}

void bar ()
{
  int *c = &a, *d = &a;
  for (b = 0; b < 12; b++)
    *d |= 1;
  foo (*c);
  baz (*c && 1);
}
