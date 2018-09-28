/* { dg-do compile { target vect_int } } */
/* { dg-additional-options "-std=c99 -fopt-info-vec -O3" } */

void
vadd (int *dst, int *op1, int *op2, int count)
{
/* { dg-optimized "loop vectorized" "" { target *-*-* } .+2 } */
/* { dg-optimized "loop versioned for vectorization because of possible aliasing" "" { target *-*-* } .+1 } */
  for (int i = 0; i < count; ++i)
    dst[i] = op1[i] + op2[i];
}
