/* Test for __imag__ side effects; see PR 33192.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int i, j;
  i = 1;
  j = __imag__ ++i;
  if (i != 2 || j != 0)
    abort ();
  return 0;
}
