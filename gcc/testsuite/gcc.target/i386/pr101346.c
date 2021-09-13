/* PR target/101346 */
/* { dg-do compile } */
/* { dg-options "-O0 -fprofile-generate -msse" } */
/* { dg-require-profiling "-fprofile-generate" } */

_Decimal128
foo (_Decimal128 x)
{
  return - __builtin_fabsd128 (x);
}
