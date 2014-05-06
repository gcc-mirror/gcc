extern void abort (void);

int32x2x2_t
test_vuzps32 (int32x2_t _a, int32x2_t _b)
{
  return vuzp_s32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int32_t first[] = {1, 2};
  int32_t second[] = {3, 4};
  int32x2x2_t result = test_vuzps32 (vld1_s32 (first), vld1_s32 (second));
  int32_t exp1[] = {1, 3};
  int32_t exp2[] = {2, 4};
  int32x2_t expect1 = vld1_s32 (exp1);
  int32x2_t expect2 = vld1_s32 (exp2);

  for (i = 0; i < 2; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
