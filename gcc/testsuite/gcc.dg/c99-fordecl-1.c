/* Test for C99 declarations in for loops.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int j = 0;
  int i = -1;
  for (int i = 1; i <= 10; i++)
    j += i;
  if (j != 55)
    abort ();
  if (i != -1)
    abort ();
  j = 0;
  for (auto int i = 1; i <= 10; i++)
    j += i;
  if (j != 55)
    abort ();
  if (i != -1)
    abort ();
  j = 0;
  for (register int i = 1; i <= 10; i++)
    j += i;
  if (j != 55)
    abort ();
  if (i != -1)
    abort ();
  exit (0);
}
