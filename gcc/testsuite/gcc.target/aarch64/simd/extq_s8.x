extern void abort (void);

int8x16_t
test_vextq_s8_1 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 1);
}

int8x16_t
test_vextq_s8_2 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 2);
}

int8x16_t
test_vextq_s8_3 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 3);
}

int8x16_t
test_vextq_s8_4 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 4);
}

int8x16_t
test_vextq_s8_5 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 5);
}

int8x16_t
test_vextq_s8_6 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 6);
}

int8x16_t
test_vextq_s8_7 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 7);
}

int8x16_t
test_vextq_s8_8 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 8);
}

int8x16_t
test_vextq_s8_9 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 9);
}

int8x16_t
test_vextq_s8_10 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 10);
}

int8x16_t
test_vextq_s8_11 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 11);
}

int8x16_t
test_vextq_s8_12 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 12);
}

int8x16_t
test_vextq_s8_13 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 13);
}

int8x16_t
test_vextq_s8_14 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 14);
}

int8x16_t
test_vextq_s8_15 (int8x16_t a, int8x16_t b)
{
  return vextq_s8 (a, b, 15);
}

int
main (int argc, char **argv)
{
  int i;
  int8_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  int8x16_t in1 = vld1q_s8 (arr1);
  int8_t arr2[] =
      {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  int8x16_t in2 = vld1q_s8 (arr2);
  int8_t exp[16];
  int8x16_t expected;
  int8x16_t actual = test_vextq_s8_1 (in1, in2);

  for (i = 0; i < 16; i++)
    exp[i] = i + 1;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_2 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 2;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_3 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 3;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_4 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 4;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_5 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 5;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_6 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 6;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_7 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 7;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_8 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 8;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_9 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 9;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_10 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 10;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_11 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 11;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_12 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 12;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_13 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 13;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_14 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 14;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s8_15 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 15;
  expected = vld1q_s8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

