/* PR target/107628 */
/* { dg-do compile } */
/* { dg-options "-fsignaling-nans -msse2" } */

typedef __bf16 __attribute__((__vector_size__ (2))) V;

void
foo (V v)
{
  v < (V) (short) 65436;
}
