/* PR c/48685 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
main ()
{
  int v = 1;
  (void) (1 == 2 ? (void) 0 : (v = 0));
  return v;
}
