extern void abort (void);

float32x4_t
test_vextq_f32_1 (float32x4_t a, float32x4_t b)
{
  return vextq_f32 (a, b, 1);
}

float32x4_t
test_vextq_f32_2 (float32x4_t a, float32x4_t b)
{
  return vextq_f32 (a, b, 2);
}

float32x4_t
test_vextq_f32_3 (float32x4_t a, float32x4_t b)
{
  return vextq_f32 (a, b, 3);
}

int
main (int argc, char **argv)
{
  int i, off;
  float32_t arr1[] = {0, 1, 2, 3};
  float32x4_t in1 = vld1q_f32 (arr1);
  float32_t arr2[] = {4, 5, 6, 7};
  float32x4_t in2 = vld1q_f32 (arr2);
  float32_t exp[4];
  float32x4_t expected;
  float32x4_t actual = test_vextq_f32_1 (in1, in2);

  for (i = 0; i < 4; i++)
    exp[i] = i + 1;
  expected = vld1q_f32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_f32_2 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 2;
  expected = vld1q_f32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_f32_3 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 3;
  expected = vld1q_f32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

