/* Check for no "noreturn" warning in main. */
/* { dg-do compile } */
/* { dg-options "-O2 -Wmissing-noreturn -fhosted" } */
extern void exit (int) __attribute__ ((__noreturn__));

int
main (void)
{
  exit (0);
}
