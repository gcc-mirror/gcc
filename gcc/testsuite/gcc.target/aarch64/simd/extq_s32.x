extern void abort (void);

int32x4_t
test_vextq_s32_1 (int32x4_t a, int32x4_t b)
{
  return vextq_s32 (a, b, 1);
}

int32x4_t
test_vextq_s32_2 (int32x4_t a, int32x4_t b)
{
  return vextq_s32 (a, b, 2);
}

int32x4_t
test_vextq_s32_3 (int32x4_t a, int32x4_t b)
{
  return vextq_s32 (a, b, 3);
}

int
main (int argc, char **argv)
{
  int i, off;
  int32_t arr1[] = {0, 1, 2, 3};
  int32x4_t in1 = vld1q_s32 (arr1);
  int32_t arr2[] = {4, 5, 6, 7};
  int32x4_t in2 = vld1q_s32 (arr2);
  int32_t exp[4];
  int32x4_t expected;
  int32x4_t actual = test_vextq_s32_1 (in1, in2);

  for (i = 0; i < 4; i++)
    exp[i] = i + 1;
  expected = vld1q_s32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s32_2 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 2;
  expected = vld1q_s32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s32_3 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 3;
  expected = vld1q_s32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

