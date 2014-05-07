extern void abort (void);

uint32x4x2_t
test_vtrnqu32 (uint32x4_t _a, uint32x4_t _b)
{
  return vtrnq_u32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint32_t first[] = {1, 2, 3, 4};
  uint32_t second[] = {5, 6, 7, 8};
  uint32x4x2_t result = test_vtrnqu32 (vld1q_u32 (first), vld1q_u32 (second));
  uint32x4_t res1 = result.val[0], res2 = result.val[1];
  uint32_t exp1[] = {1, 5, 3, 7};
  uint32_t exp2[] = {2, 6, 4, 8};
  uint32x4_t expected1 = vld1q_u32 (exp1);
  uint32x4_t expected2 = vld1q_u32 (exp2);

  for (i = 0; i < 4; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
