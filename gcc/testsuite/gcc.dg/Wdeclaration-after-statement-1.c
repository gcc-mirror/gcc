/* Test for -Wdeclaration-after-statement emitting warnings when no
   standard-specifying option is given.  See also c9?-mixdecl-*.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-Wdeclaration-after-statement" } */

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
  int j = i;				/* { dg-warning "" "declaration after statement" } */
  if (j != 1)
    abort ();
  struct foo { int i0; } k = { 4 };	/* { dg-warning "" "declaration after statement" } */
  if (k.i0 != 4)
    abort ();
  exit (0);
}
