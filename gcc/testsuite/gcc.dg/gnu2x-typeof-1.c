/* Test __typeof__ propagation of noreturn function attributes with -std=gnu2x:
   these are part of the type of a function pointer with GNU __typeof__, but
   not with C2x typeof.  */
/* { dg-do link } */
/* { dg-options "-std=gnu2x -O2" } */

_Noreturn void f (void);

__typeof__ (&f) volatile p;
__typeof__ (&p) volatile pp;

void link_failure (void);

void
g (void)
{
  (*p) ();
  link_failure ();
}

void
h (void)
{
  (**pp) ();
  link_failure ();
}

volatile int flag;
volatile int x;

int
main (void)
{
  if (flag)
    g ();
  if (flag)
    h ();
  return x;
}
