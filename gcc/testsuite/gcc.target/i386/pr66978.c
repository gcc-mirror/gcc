/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=short" } */

extern int foo (int *);
int
bar (int *p)
{
  __attribute__ ((noinline, noclone))
  int hack_digit (void)
    {
      return foo (p);
    }
  return hack_digit ();
}
