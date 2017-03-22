/* PR rtl-optimization/79574 */
/* { dg-do compile } */
/* { dg-options "-Os --param gcse-cost-distance-ratio=2147483647" } */

void a (void)
{
  volatile int b;
  for (;; b)
    ;
}
