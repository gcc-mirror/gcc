extern void abort (void);

uint32x4_t
test_vextq_u32_1 (uint32x4_t a, uint32x4_t b)
{
  return vextq_u32 (a, b, 1);
}

uint32x4_t
test_vextq_u32_2 (uint32x4_t a, uint32x4_t b)
{
  return vextq_u32 (a, b, 2);
}

uint32x4_t
test_vextq_u32_3 (uint32x4_t a, uint32x4_t b)
{
  return vextq_u32 (a, b, 3);
}

int
main (int argc, char **argv)
{
  int i, off;
  uint32_t arr1[] = {0, 1, 2, 3};
  uint32x4_t in1 = vld1q_u32 (arr1);
  uint32_t arr2[] = {4, 5, 6, 7};
  uint32x4_t in2 = vld1q_u32 (arr2);
  uint32_t exp[4];
  uint32x4_t expected;
  uint32x4_t actual = test_vextq_u32_1 (in1, in2);

  for (i = 0; i < 4; i++)
    exp[i] = i + 1;
  expected = vld1q_u32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u32_2 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 2;
  expected = vld1q_u32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u32_3 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 3;
  expected = vld1q_u32 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

