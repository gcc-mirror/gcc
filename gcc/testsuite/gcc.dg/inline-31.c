/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */
inline int f (void) { return 0; }

void
g (void)
{
  extern int f();
}
