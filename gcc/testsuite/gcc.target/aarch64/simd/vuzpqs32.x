extern void abort (void);

int32x4x2_t
test_vuzpqs32 (int32x4_t _a, int32x4_t _b)
{
  return vuzpq_s32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int32_t first[] = {1, 2, 3, 4};
  int32_t second[] = {5, 6, 7, 8};
  int32x4x2_t result = test_vuzpqs32 (vld1q_s32 (first), vld1q_s32 (second));
  int32_t exp1[] = {1, 3, 5, 7};
  int32_t exp2[] = {2, 4, 6, 8};
  int32x4_t expect1 = vld1q_s32 (exp1);
  int32x4_t expect2 = vld1q_s32 (exp2);

  for (i = 0; i < 4; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
