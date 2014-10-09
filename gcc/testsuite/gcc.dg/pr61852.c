/* PR c/61852 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-function-declaration" } */

int
f (int a)
{
  int b = a + a + a + ff (a); /* { dg-warning "23:implicit declaration of function" } */
  return b;
}
