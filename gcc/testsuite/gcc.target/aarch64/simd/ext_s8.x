extern void abort (void);

int8x8_t
test_vext_s8_1 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 1);
}

int8x8_t
test_vext_s8_2 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 2);
}

int8x8_t
test_vext_s8_3 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 3);
}

int8x8_t
test_vext_s8_4 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 4);
}

int8x8_t
test_vext_s8_5 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 5);
}

int8x8_t
test_vext_s8_6 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 6);
}

int8x8_t
test_vext_s8_7 (int8x8_t a, int8x8_t b)
{
  return vext_s8 (a, b, 7);
}

int
main (int argc, char **argv)
{
  int i, off;
  int8_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7};
  int8x8_t in1 = vld1_s8 (arr1);
  int8_t arr2[] = {8, 9, 10, 11, 12, 13, 14, 15};
  int8x8_t in2 = vld1_s8 (arr2);
  int8_t exp[8];
  int8x8_t expected;
  int8x8_t actual = test_vext_s8_1 (in1, in2);

  for (i = 0; i < 8; i++)
    exp[i] = i + 1;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s8_2 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 2;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s8_3 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 3;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s8_4 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 4;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s8_5 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 5;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s8_6 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 6;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_s8_7 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 7;
  expected = vld1_s8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

