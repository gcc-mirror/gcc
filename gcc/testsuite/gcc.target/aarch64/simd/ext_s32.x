extern void abort (void);

int32x2_t
test_vext_s32_1 (int32x2_t a, int32x2_t b)
{
  return vext_s32 (a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  int32_t arr1[] = {0, 1};
  int32x2_t in1 = vld1_s32 (arr1);
  int32_t arr2[] = {2, 3};
  int32x2_t in2 = vld1_s32 (arr2);
  int32_t exp[2];
  int32x2_t expected;
  int32x2_t actual = test_vext_s32_1 (in1, in2);

  for (i = 0; i < 2; i++)
    exp[i] = i + 1;
  expected = vld1_s32 (exp);
  for (i = 0; i < 2; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

