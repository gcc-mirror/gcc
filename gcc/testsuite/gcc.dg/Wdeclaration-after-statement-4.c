/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic -Wno-declaration-after-statement" } */

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
  int j = i;
  if (j != 1)
    abort ();
  struct foo { int i0; } k = { 4 };
  if (k.i0 != 4)
    abort ();
  exit (0);
}
