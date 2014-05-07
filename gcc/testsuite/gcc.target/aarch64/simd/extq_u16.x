extern void abort (void);

uint16x8_t
test_vextq_u16_1 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 1);
}

uint16x8_t
test_vextq_u16_2 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 2);
}

uint16x8_t
test_vextq_u16_3 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 3);
}

uint16x8_t
test_vextq_u16_4 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 4);
}

uint16x8_t
test_vextq_u16_5 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 5);
}

uint16x8_t
test_vextq_u16_6 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 6);
}

uint16x8_t
test_vextq_u16_7 (uint16x8_t a, uint16x8_t b)
{
  return vextq_u16 (a, b, 7);
}

int
main (int argc, char **argv)
{
  int i, off;
  uint16_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7};
  uint16x8_t in1 = vld1q_u16 (arr1);
  uint16_t arr2[] = {8, 9, 10, 11, 12, 13, 14, 15};
  uint16x8_t in2 = vld1q_u16 (arr2);
  uint16_t exp[8];
  uint16x8_t expected;
  uint16x8_t actual = test_vextq_u16_1 (in1, in2);

  for (i = 0; i < 8; i++)
    exp[i] = i + 1;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u16_2 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 2;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u16_3 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 3;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u16_4 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 4;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u16_5 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 5;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u16_6 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 6;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u16_7 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 7;
  expected = vld1q_u16 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

