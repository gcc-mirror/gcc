extern void abort (void);

float32x4x2_t
test_vuzpqf32 (float32x4_t _a, float32x4_t _b)
{
  return vuzpq_f32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  float32_t first[] = {1, 2, 3, 4};
  float32_t second[] = {5, 6, 7, 8};
  float32x4x2_t result = test_vuzpqf32 (vld1q_f32 (first), vld1q_f32 (second));
  float32_t exp1[] = {1, 3, 5, 7};
  float32_t exp2[] = {2, 4, 6, 8};
  float32x4_t expect1 = vld1q_f32 (exp1);
  float32x4_t expect2 = vld1q_f32 (exp2);

  for (i = 0; i < 4; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
