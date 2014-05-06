extern void abort (void);

int16x4x2_t
test_vuzps16 (int16x4_t _a, int16x4_t _b)
{
  return vuzp_s16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int16_t first[] = {1, 2, 3, 4};
  int16_t second[] = {5, 6, 7, 8};
  int16x4x2_t result = test_vuzps16 (vld1_s16 (first), vld1_s16 (second));
  int16_t exp1[] = {1, 3, 5, 7};
  int16_t exp2[] = {2, 4, 6, 8};
  int16x4_t expect1 = vld1_s16 (exp1);
  int16x4_t expect2 = vld1_s16 (exp2);

  for (i = 0; i < 4; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
