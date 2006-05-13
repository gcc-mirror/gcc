/* { dg-do compile } */
/* { dg-do run } */
/* { dg-options "-Os" } */

unsigned int
foo (unsigned int x)
{
  unsigned int r = x;
  while (--x)
    r *= x;
  return r;
}

unsigned long long
bar (unsigned long long x)
{
  unsigned long long r = x;
  while (--x)
    r *= x;
  return r;
}

extern void abort (void);

int
main (void)
{
  if (foo (5) != 120 || bar (5) != 120)
    abort ();
  return 0;
}
