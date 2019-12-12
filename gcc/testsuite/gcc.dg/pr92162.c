/* { dg-do compile } */
/* { dg-options "-Ofast" } */

short int s8;

void __attribute__ ((simd))
gn (void)
{
  s8 = 0;
}
