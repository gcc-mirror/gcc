extern void abort (void);

uint32x2_t
test_vext_u32_1 (uint32x2_t a, uint32x2_t b)
{
  return vext_u32 (a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  uint32_t arr1[] = {0, 1};
  uint32x2_t in1 = vld1_u32 (arr1);
  uint32_t arr2[] = {2, 3};
  uint32x2_t in2 = vld1_u32 (arr2);
  uint32_t exp[2];
  uint32x2_t expected;
  uint32x2_t actual = test_vext_u32_1 (in1, in2);

  for (i = 0; i < 2; i++)
    exp[i] = i + 1;
  expected = vld1_u32 (exp);
  for (i = 0; i < 2; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

