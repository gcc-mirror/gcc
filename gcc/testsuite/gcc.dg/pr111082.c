/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model" } */
/* { dg-additional-options "-mavx512f" { target { x86_64-*-* i?86-*-* } } } */

long minarray2(const long *input)
{
  if (input[0] < input[1])
    return input[0] ;
  return input[1];
}
