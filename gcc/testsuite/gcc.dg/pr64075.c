/* PR lto/64075 */
/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto" } */

_Complex float test (float a, float b, float c, float d)
{
  return 1.0iF;
}
