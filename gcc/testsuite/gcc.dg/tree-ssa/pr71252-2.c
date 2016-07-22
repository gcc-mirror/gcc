/* PR middle-end/71252 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned a;
int b, c;
void fn1 ()
{
  b = a + c + 3 + c;
}
