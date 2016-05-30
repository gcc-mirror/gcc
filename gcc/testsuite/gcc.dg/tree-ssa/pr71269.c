/* PR middle-end/71269 */
/* { dg-do compile } */
/* { dg-options "-O1" } */

int a, b, c;
void  fn2 (int);
void fn1 ()
{
  fn2 (sizeof 0 + c + a + b + b);
}
