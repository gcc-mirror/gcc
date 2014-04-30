extern void abort (void);

uint16x4x2_t
test_vuzpu16 (uint16x4_t _a, uint16x4_t _b)
{
  return vuzp_u16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint16_t first[] = {1, 2, 3, 4};
  uint16_t second[] = {5, 6, 7, 8};
  uint16x4x2_t result = test_vuzpu16 (vld1_u16 (first), vld1_u16 (second));
  uint16_t exp1[] = {1, 3, 5, 7};
  uint16_t exp2[] = {2, 4, 6, 8};
  uint16x4_t expect1 = vld1_u16 (exp1);
  uint16x4_t expect2 = vld1_u16 (exp2);

  for (i = 0; i < 4; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
