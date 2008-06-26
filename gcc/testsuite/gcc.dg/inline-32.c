/* { dg-do link } */
/* { dg-options "-std=c99 -pedantic-errors" } */
/* { dg-additional-sources inline-32a.c } */
inline int f (void) { return 0; }

int
main (void)
{
  extern int f();
  return f ();
}
