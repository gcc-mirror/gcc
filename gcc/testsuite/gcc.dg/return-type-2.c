/* Bogus warnings claiming we fall off the end of a non-void function.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/27/2000.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wreturn-type" } */

extern void abort (void) __attribute__ ((__noreturn__));

int
foo1 (int i)
{
  if (i)
    return i;

  abort ();
} /* { dg-bogus "control reaches end of non-void function" "warning for falling off end of non-void function" } */

__inline__ int
foo2 (int i)
{
  if (i)
    return i;

  abort ();
} /* { dg-bogus "control reaches end of non-void function" "warning for falling off end of non-void function" } */

static int
foo3 (int i)
{
  if (i)
    return i;

  abort ();
} /* { dg-bogus "control reaches end of non-void function" "warning for falling off end of non-void function" } */

static __inline__ int
foo4 (int i)
{
  if (i)
    return i;

  abort ();
} /* { dg-bogus "control reaches end of non-void function" "warning for falling off end of non-void function" } */

int bar (int i)
{
  return foo1 (i) + foo2 (i) + foo3 (i) + foo4 (i);
}
