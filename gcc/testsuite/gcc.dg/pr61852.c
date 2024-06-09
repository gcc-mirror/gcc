/* PR c/61852 */
/* { dg-do compile } */
/* { dg-options "" } */

int
f (int a)
{
  int b = a + a + a + ff (a); /* { dg-error "23:implicit declaration of function" } */
  return b;
}
