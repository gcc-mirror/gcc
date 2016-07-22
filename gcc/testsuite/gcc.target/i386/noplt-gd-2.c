/* { dg-do run { target { *-*-linux* } } } */
/* { dg-options "-O2 -fpic -fno-plt" } */
/* { dg-additional-sources noplt-gd-1.c } */

__thread int gd = 1;
extern void abort (void);
extern int * get_gd (void);
extern void set_gd (int);
extern int test_gd (int);

int
main ()
{
  int *p;
 
  if (gd != 1)
    abort ();

  p = get_gd ();
  if (*p != gd)
    abort ();

  set_gd (4);
  if (*p != 4 || !test_gd (4))
    abort ();

  return 0;
}
