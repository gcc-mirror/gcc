extern void abort (void);

poly16x8_t
test_vextq_p16_1 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 1);
}

poly16x8_t
test_vextq_p16_2 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 2);
}

poly16x8_t
test_vextq_p16_3 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 3);
}

poly16x8_t
test_vextq_p16_4 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 4);
}

poly16x8_t
test_vextq_p16_5 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 5);
}

poly16x8_t
test_vextq_p16_6 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 6);
}

poly16x8_t
test_vextq_p16_7 (poly16x8_t a, poly16x8_t b)
{
  return vextq_p16 (a, b, 7);
}

int
main (int argc, char **argv)
{
  int i, off;
  poly16_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7};
  poly16x8_t in1 = vld1q_p16 (arr1);
  poly16_t arr2[] = {8, 9, 10, 11, 12, 13, 14, 15};
  poly16x8_t in2 = vld1q_p16 (arr2);
  poly16_t exp[8];
  poly16x8_t expected;
  poly16x8_t actual = test_vextq_p16_1 (in1, in2);

  for (i = 0; i < 8; i++)
    exp[i] = i + 1;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p16_2 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 2;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p16_3 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 3;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p16_4 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 4;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p16_5 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 5;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p16_6 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 6;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_p16_7 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 7;
  expected = vld1q_p16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

