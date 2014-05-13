extern void abort (void);

float32x2_t
test_vext_f32_1 (float32x2_t a, float32x2_t b)
{
  return vext_f32 (a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  float32_t arr1[] = {0, 1};
  float32x2_t in1 = vld1_f32 (arr1);
  float32_t arr2[] = {2, 3};
  float32x2_t in2 = vld1_f32 (arr2);
  float32_t exp[2];
  float32x2_t expected;
  float32x2_t actual = test_vext_f32_1 (in1, in2);

  for (i = 0; i < 2; i++)
    exp[i] = i + 1;
  expected = vld1_f32 (exp);
  for (i = 0; i < 2; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

