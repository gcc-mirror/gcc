extern void abort (void);

uint8x16_t
test_vextq_u8_1 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 1);
}

uint8x16_t
test_vextq_u8_2 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 2);
}

uint8x16_t
test_vextq_u8_3 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 3);
}

uint8x16_t
test_vextq_u8_4 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 4);
}

uint8x16_t
test_vextq_u8_5 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 5);
}

uint8x16_t
test_vextq_u8_6 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 6);
}

uint8x16_t
test_vextq_u8_7 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 7);
}

uint8x16_t
test_vextq_u8_8 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 8);
}

uint8x16_t
test_vextq_u8_9 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 9);
}

uint8x16_t
test_vextq_u8_10 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 10);
}

uint8x16_t
test_vextq_u8_11 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 11);
}

uint8x16_t
test_vextq_u8_12 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 12);
}

uint8x16_t
test_vextq_u8_13 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 13);
}

uint8x16_t
test_vextq_u8_14 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 14);
}

uint8x16_t
test_vextq_u8_15 (uint8x16_t a, uint8x16_t b)
{
  return vextq_u8 (a, b, 15);
}

int
main (int argc, char **argv)
{
  int i;
  uint8_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  uint8x16_t in1 = vld1q_u8 (arr1);
  uint8_t arr2[] =
      {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  uint8x16_t in2 = vld1q_u8 (arr2);
  uint8_t exp[16];
  uint8x16_t expected;
  uint8x16_t actual = test_vextq_u8_1 (in1, in2);

  for (i = 0; i < 16; i++)
    exp[i] = i + 1;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_2 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 2;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_3 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 3;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_4 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 4;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_5 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 5;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_6 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 6;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_7 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 7;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_8 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 8;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_9 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 9;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_10 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 10;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_11 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 11;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_12 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 12;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_13 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 13;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_14 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 14;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vextq_u8_15 (in1, in2);
  for (i = 0; i < 16; i++)
    exp[i] = i + 15;
  expected = vld1q_u8 (exp);
  for (i = 0; i < 16; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

