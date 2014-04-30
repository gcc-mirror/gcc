extern void abort (void);

uint8x8x2_t
test_vzipu8 (uint8x8_t _a, uint8x8_t _b)
{
  return vzip_u8 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint8_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  uint8_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  uint8x8x2_t result = test_vzipu8 (vld1_u8 (first), vld1_u8 (second));
  uint8x8_t res1 = result.val[0], res2 = result.val[1];
  uint8_t exp1[] = {1, 9, 2, 10, 3, 11, 4, 12};
  uint8_t exp2[] = {5, 13, 6, 14, 7, 15, 8, 16};
  uint8x8_t expected1 = vld1_u8 (exp1);
  uint8x8_t expected2 = vld1_u8 (exp2);

  for (i = 0; i < 8; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
