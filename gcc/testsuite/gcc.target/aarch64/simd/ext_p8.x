extern void abort (void);

poly8x8_t
test_vext_p8_1 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 1);
}

poly8x8_t
test_vext_p8_2 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 2);
}

poly8x8_t
test_vext_p8_3 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 3);
}

poly8x8_t
test_vext_p8_4 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 4);
}

poly8x8_t
test_vext_p8_5 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 5);
}

poly8x8_t
test_vext_p8_6 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 6);
}

poly8x8_t
test_vext_p8_7 (poly8x8_t a, poly8x8_t b)
{
  return vext_p8 (a, b, 7);
}

int
main (int argc, char **argv)
{
  int i, off;
  poly8_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7};
  poly8x8_t in1 = vld1_p8 (arr1);
  poly8_t arr2[] = {8, 9, 10, 11, 12, 13, 14, 15};
  poly8x8_t in2 = vld1_p8 (arr2);
  poly8_t exp[8];
  poly8x8_t expected;
  poly8x8_t actual = test_vext_p8_1 (in1, in2);

  for (i = 0; i < 8; i++)
    exp[i] = i + 1;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p8_2 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 2;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p8_3 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 3;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p8_4 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 4;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p8_5 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 5;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p8_6 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 6;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_p8_7 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 7;
  expected = vld1_p8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

