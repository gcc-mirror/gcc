extern void abort (void);

poly16x4_t
test_vext_p16_1 (poly16x4_t a, poly16x4_t b)
{
  return vext_p16 (a, b, 1);
}

poly16x4_t
test_vext_p16_2 (poly16x4_t a, poly16x4_t b)
{
  return vext_p16 (a, b, 2);
}

poly16x4_t
test_vext_p16_3 (poly16x4_t a, poly16x4_t b)
{
  return vext_p16 (a, b, 3);
}

int
main (int argc, char **argv)
{
  int i, off;
  poly16_t arr1[] = {0, 1, 2, 3};
  poly16x4_t in1 = vld1_p16 (arr1);
  poly16_t arr2[] = {4, 5, 6, 7};
  poly16x4_t in2 = vld1_p16 (arr2);
  poly16_t exp[4];
  poly16x4_t expected;
  poly16x4_t actual = test_vext_p16_1 (in1, in2);

  for (i = 0; i < 4; i++)
    exp[i] = i + 1;
  expected = vld1_p16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p16_2 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 2;
  expected = vld1_p16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p16_3 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 3;
  expected = vld1_p16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

