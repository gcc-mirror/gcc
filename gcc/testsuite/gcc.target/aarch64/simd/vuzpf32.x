extern void abort (void);

float32x2x2_t
test_vuzpf32 (float32x2_t _a, float32x2_t _b)
{
  return vuzp_f32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  float32_t first[] = {1, 2};
  float32_t second[] = {3, 4};
  float32x2x2_t result = test_vuzpf32 (vld1_f32 (first), vld1_f32 (second));
  float32_t exp1[] = {1, 3};
  float32_t exp2[] = {2, 4};
  float32x2_t expect1 = vld1_f32 (exp1);
  float32x2_t expect2 = vld1_f32 (exp2);

  for (i = 0; i < 2; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
