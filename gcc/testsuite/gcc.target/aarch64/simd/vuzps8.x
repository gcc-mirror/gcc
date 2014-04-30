extern void abort (void);

int8x8x2_t
test_vuzps8 (int8x8_t _a, int8x8_t _b)
{
  return vuzp_s8 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int8_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  int8_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  int8x8x2_t result = test_vuzps8 (vld1_s8 (first), vld1_s8 (second));
  int8_t exp1[] = {1, 3, 5, 7, 9, 11, 13, 15};
  int8_t exp2[] = {2, 4, 6, 8, 10, 12, 14, 16};
  int8x8_t expect1 = vld1_s8 (exp1);
  int8x8_t expect2 = vld1_s8 (exp2);

  for (i = 0; i < 8; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
