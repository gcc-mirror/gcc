/* { dg-options "-std=c99 -fopt-info -O3" } */

void
vadd (int *dst, int *op1, int *op2, int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = op1[i] + op2[i];
}

/* { dg-message "loop vectorized" "" { target *-*-* } 6 } */
/* { dg-message "loop versioned for vectorization because of possible aliasing" "" { target *-*-* } 6 } */
