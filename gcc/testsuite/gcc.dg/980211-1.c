/* Test long double on x86. */

/* { dg-do run { target i?86-*-* } } */
/* { dg-options -O2 } */

__inline int
__signbitl (long double __x)
{
  union { long double __l; int __i[3]; } __u = { __l: __x };

  return (__u.__i[2] & 0x8000) != 0;
}

void
foo (long double x, long double y)
{
  long double z = x / y;
  if (__signbitl (x) && __signbitl (z))
    abort ();
}

int main()
{
  if (sizeof (long double) > sizeof (double))
    foo (-0.0, -1.0);
  return 0;
}
