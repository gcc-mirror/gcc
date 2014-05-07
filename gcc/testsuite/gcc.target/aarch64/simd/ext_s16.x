extern void abort (void);

int16x4_t
test_vext_s16_1 (int16x4_t a, int16x4_t b)
{
  return vext_s16 (a, b, 1);
}

int16x4_t
test_vext_s16_2 (int16x4_t a, int16x4_t b)
{
  return vext_s16 (a, b, 2);
}

int16x4_t
test_vext_s16_3 (int16x4_t a, int16x4_t b)
{
  return vext_s16 (a, b, 3);
}

int
main (int argc, char **argv)
{
  int i, off;
  int16_t arr1[] = {0, 1, 2, 3};
  int16x4_t in1 = vld1_s16 (arr1);
  int16_t arr2[] = {4, 5, 6, 7};
  int16x4_t in2 = vld1_s16 (arr2);
  int16_t exp[4];
  int16x4_t expected;
  int16x4_t actual = test_vext_s16_1 (in1, in2);

  for (i = 0; i < 4; i++)
    exp[i] = i + 1;
  expected = vld1_s16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s16_2 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 2;
  expected = vld1_s16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s16_3 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 3;
  expected = vld1_s16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

