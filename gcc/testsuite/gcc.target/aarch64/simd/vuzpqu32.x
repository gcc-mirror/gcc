extern void abort (void);

uint32x4x2_t
test_vuzpqu32 (uint32x4_t _a, uint32x4_t _b)
{
  return vuzpq_u32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint32_t first[] = {1, 2, 3, 4};
  uint32_t second[] = {5, 6, 7, 8};
  uint32x4x2_t result = test_vuzpqu32 (vld1q_u32 (first), vld1q_u32 (second));
  uint32_t exp1[] = {1, 3, 5, 7};
  uint32_t exp2[] = {2, 4, 6, 8};
  uint32x4_t expect1 = vld1q_u32 (exp1);
  uint32x4_t expect2 = vld1q_u32 (exp2);

  for (i = 0; i < 4; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
