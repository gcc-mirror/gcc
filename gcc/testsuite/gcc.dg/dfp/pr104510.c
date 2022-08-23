/* PR c/104510 */
/* { dg-do compile } */
/* { dg-options "" } */

float f;
_Decimal64 d;

int
foo (void)
{
  return d > (_Decimal32) (_Decimal64) f;
}
