/* Test for C99 __func__: not merging with string literals.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  if ("main" == __func__)
    abort ();
  else
    exit (0);
}
