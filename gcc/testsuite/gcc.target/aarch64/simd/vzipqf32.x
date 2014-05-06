extern void abort (void);

float32x4x2_t
test_vzipqf32 (float32x4_t _a, float32x4_t _b)
{
  return vzipq_f32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  float32_t first[] = {1, 2, 3, 4};
  float32_t second[] = {5, 6, 7, 8};
  float32x4x2_t result = test_vzipqf32 (vld1q_f32 (first), vld1q_f32 (second));
  float32x4_t res1 = result.val[0], res2 = result.val[1];
  float32_t exp1[] = {1, 5, 2, 6};
  float32_t exp2[] = {3, 7, 4, 8};
  float32x4_t expected1 = vld1q_f32 (exp1);
  float32x4_t expected2 = vld1q_f32 (exp2);

  for (i = 0; i < 4; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
