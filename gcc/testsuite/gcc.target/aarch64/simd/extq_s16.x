extern void abort (void);

int16x8_t
test_vextq_s16_1 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 1);
}

int16x8_t
test_vextq_s16_2 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 2);
}

int16x8_t
test_vextq_s16_3 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 3);
}

int16x8_t
test_vextq_s16_4 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 4);
}

int16x8_t
test_vextq_s16_5 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 5);
}

int16x8_t
test_vextq_s16_6 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 6);
}

int16x8_t
test_vextq_s16_7 (int16x8_t a, int16x8_t b)
{
  return vextq_s16 (a, b, 7);
}

int
main (int argc, char **argv)
{
  int i, off;
  int16_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7};
  int16x8_t in1 = vld1q_s16 (arr1);
  int16_t arr2[] = {8, 9, 10, 11, 12, 13, 14, 15};
  int16x8_t in2 = vld1q_s16 (arr2);
  int16_t exp[8];
  int16x8_t expected;
  int16x8_t actual = test_vextq_s16_1 (in1, in2);

  for (i = 0; i < 8; i++)
    exp[i] = i + 1;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s16_2 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 2;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s16_3 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 3;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s16_4 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 4;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s16_5 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 5;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s16_6 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 6;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_s16_7 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 7;
  expected = vld1q_s16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

