extern void abort (void);

uint16x4_t
test_vext_u16_1 (uint16x4_t a, uint16x4_t b)
{
  return vext_u16 (a, b, 1);
}

uint16x4_t
test_vext_u16_2 (uint16x4_t a, uint16x4_t b)
{
  return vext_u16 (a, b, 2);
}

uint16x4_t
test_vext_u16_3 (uint16x4_t a, uint16x4_t b)
{
  return vext_u16 (a, b, 3);
}

int
main (int argc, char **argv)
{
  int i, off;
  uint16_t arr1[] = {0, 1, 2, 3};
  uint16x4_t in1 = vld1_u16 (arr1);
  uint16_t arr2[] = {4, 5, 6, 7};
  uint16x4_t in2 = vld1_u16 (arr2);
  uint16_t exp[4];
  uint16x4_t expected;
  uint16x4_t actual = test_vext_u16_1 (in1, in2);

  for (i = 0; i < 4; i++)
    exp[i] = i + 1;
  expected = vld1_u16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u16_2 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 2;
  expected = vld1_u16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  actual = test_vext_u16_3 (in1, in2);
  for (i = 0; i < 4; i++)
    exp[i] = i + 3;
  expected = vld1_u16 (exp);
  for (i = 0; i < 4; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

