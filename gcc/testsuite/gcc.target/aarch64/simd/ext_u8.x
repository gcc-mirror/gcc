extern void abort (void);

uint8x8_t
test_vext_u8_1 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 1);
}

uint8x8_t
test_vext_u8_2 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 2);
}

uint8x8_t
test_vext_u8_3 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 3);
}

uint8x8_t
test_vext_u8_4 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 4);
}

uint8x8_t
test_vext_u8_5 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 5);
}

uint8x8_t
test_vext_u8_6 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 6);
}

uint8x8_t
test_vext_u8_7 (uint8x8_t a, uint8x8_t b)
{
  return vext_u8 (a, b, 7);
}

int
main (int argc, char **argv)
{
  int i, off;
  uint8_t arr1[] = {0, 1, 2, 3, 4, 5, 6, 7};
  uint8x8_t in1 = vld1_u8 (arr1);
  uint8_t arr2[] = {8, 9, 10, 11, 12, 13, 14, 15};
  uint8x8_t in2 = vld1_u8 (arr2);
  uint8_t exp[8];
  uint8x8_t expected;
  uint8x8_t actual = test_vext_u8_1 (in1, in2);

  for (i = 0; i < 8; i++)
    exp[i] = i + 1;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u8_2 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 2;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u8_3 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 3;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u8_4 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 4;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u8_5 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 5;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u8_6 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 6;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u8_7 (in1, in2);
  for (i = 0; i < 8; i++)
    exp[i] = i + 7;
  expected = vld1_u8 (exp);
  for (i = 0; i < 8; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

