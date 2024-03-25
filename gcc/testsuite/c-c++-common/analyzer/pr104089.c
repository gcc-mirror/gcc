/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */
/* { dg-additional-options "-frounding-math" } */

volatile _Float16 true_min = 5.96046447753906250000000000000000000e-8F16;

int
main (void)
{
  return __builtin_fpclassify (0, 1, 4, 3, 2, true_min);
}
