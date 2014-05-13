extern void abort (void);

poly8x16_t
test_vextq_p8_1 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 1);
}

poly8x16_t
test_vextq_p8_2 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 2);
}

poly8x16_t
test_vextq_p8_3 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 3);
}

poly8x16_t
test_vextq_p8_4 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 4);
}

poly8x16_t
test_vextq_p8_5 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 5);
}

poly8x16_t
test_vextq_p8_6 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 6);
}

poly8x16_t
test_vextq_p8_7 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 7);
}

poly8x16_t
test_vextq_p8_8 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 8);
}

poly8x16_t
test_vextq_p8_9 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 9);
}

poly8x16_t
test_vextq_p8_10 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 10);
}

poly8x16_t
test_vextq_p8_11 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 11);
}

poly8x16_t
test_vextq_p8_12 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 12);
}

poly8x16_t
test_vextq_p8_13 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 13);
}

poly8x16_t
test_vextq_p8_14 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 14);
}

poly8x16_t
test_vextq_p8_15 (poly8x16_t a, poly8x16_t b)
{
  return vextq_p8 (a, b, 15);
}

int
main (int argc, char **argv)
{
  int i;
  poly8_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  poly8x16_t in1 = vld1q_p8 (arr1);
  poly8_t arr2[] =
      {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  poly8x16_t in2 = vld1q_p8 (arr2);
  poly8_t exp[16];
  poly8x16_t expected;
  poly8x16_t actual = test_vextq_p8_1 (in1, in2);

  for (i = 0; i < 16; i++)
    exp[i] = i + 1;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_2 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 2;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_3 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 3;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_4 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 4;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_5 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 5;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_6 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 6;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_7 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 7;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_8 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 8;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_9 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 9;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_10 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 10;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_11 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 11;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_12 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 12;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_13 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 13;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_14 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 14;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p8_15 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 15;
  expected = vld1q_p8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

