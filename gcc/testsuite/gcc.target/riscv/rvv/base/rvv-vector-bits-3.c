/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64 -mrvv-vector-bits=zvl -O3" } */

void test_rvv_vector_bits_zvl (int *a, int *b, int *out)
{
  for (int i = 0; i < 8; i++)
    out[i] = a[i] + b[i];
}
