/* Check for invalid "noreturn" warning. */
/* { dg-do compile } */
/* { dg-options "-O3 -Wall" } */
extern void abort (void) __attribute__ ((__noreturn__));

void noreturn (int x) __attribute__ ((__noreturn__));

void
noreturn (int x)
{
  abort ();
} /* { dg-bogus "does return" "noreturn does return" } */
