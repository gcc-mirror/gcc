/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt" } */
/* { dg-additional-sources noplt-ld-1.c } */

extern void abort (void);
extern int * get_ld (void);
extern void set_ld (int);
extern int test_ld (int);

int
main ()
{
  int *p;
 
  p = get_ld ();
  set_ld (4);
  if (*p != 4 || !test_ld (4))
    abort ();

  return 0;
}
