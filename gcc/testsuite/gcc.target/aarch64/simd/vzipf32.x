extern void abort (void);

float32x2x2_t
test_vzipf32 (float32x2_t _a, float32x2_t _b)
{
  return vzip_f32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  float32_t first[] = {1, 2};
  float32_t second[] = {3, 4};
  float32x2x2_t result = test_vzipf32 (vld1_f32 (first), vld1_f32 (second));
  float32x2_t res1 = result.val[0], res2 = result.val[1];
  float32_t exp1[] = {1, 3};
  float32_t exp2[] = {2, 4};
  float32x2_t expected1 = vld1_f32 (exp1);
  float32x2_t expected2 = vld1_f32 (exp2);

  for (i = 0; i < 2; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
