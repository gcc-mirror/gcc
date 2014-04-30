extern void abort (void);

uint8x8x2_t
test_vuzpu8 (uint8x8_t _a, uint8x8_t _b)
{
  return vuzp_u8 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint8_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  uint8_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  uint8x8x2_t result = test_vuzpu8 (vld1_u8 (first), vld1_u8 (second));
  uint8_t exp1[] = {1, 3, 5, 7, 9, 11, 13, 15};
  uint8_t exp2[] = {2, 4, 6, 8, 10, 12, 14, 16};
  uint8x8_t expect1 = vld1_u8 (exp1);
  uint8x8_t expect2 = vld1_u8 (exp2);

  for (i = 0; i < 8; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
