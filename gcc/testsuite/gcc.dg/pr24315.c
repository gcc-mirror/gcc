/* PR target/24315 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O0 -fpeephole2" } */

void s48_double_to_bignum (int exponent)
{
  long length = ((((exponent) + ((((sizeof (long)) * 8) - 2) - 1)) /
		  (((sizeof (long)) * 8) - 2)));
}
