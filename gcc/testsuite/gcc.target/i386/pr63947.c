/* PR target/63947 */
/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */

long double foo (unsigned a, unsigned b)
{
  return a + b < a;
}
