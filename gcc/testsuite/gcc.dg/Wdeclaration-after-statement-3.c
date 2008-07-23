/* PR 35058: -Werror= works only with some warnings. */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic -Werror=declaration-after-statement" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int i = 0;
  if (i != 0)
    abort ();
  i++;
  if (i != 1)
    abort ();
  int j = i;				/* { dg-error "" "declaration-after-statement" } */
  if (j != 1)
    abort ();
  struct foo { int i0; } k = { 4 };	/* { dg-error "" "declaration-after-statement" } */
  if (k.i0 != 4)
    abort ();
  exit (0);
}
