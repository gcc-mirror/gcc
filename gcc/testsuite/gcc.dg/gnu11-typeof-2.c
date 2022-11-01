/* Test typeof propagation of noreturn function attributes with -std=gnu11:
   these are part of the type of a function pointer with GNU typeof, but not
   with C2x typeof.  */
/* { dg-do link } */
/* { dg-options "-std=gnu11 -O2" } */

_Noreturn void f (void);

typeof (&f) volatile p;
typeof (&p) volatile pp;

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
