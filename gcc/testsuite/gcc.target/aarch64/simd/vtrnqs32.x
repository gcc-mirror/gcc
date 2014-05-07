extern void abort (void);

int32x4x2_t
test_vtrnqs32 (int32x4_t _a, int32x4_t _b)
{
  return vtrnq_s32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int32_t first[] = {1, 2, 3, 4};
  int32_t second[] = {5, 6, 7, 8};
  int32x4x2_t result = test_vtrnqs32 (vld1q_s32 (first), vld1q_s32 (second));
  int32x4_t res1 = result.val[0], res2 = result.val[1];
  int32_t exp1[] = {1, 5, 3, 7};
  int32_t exp2[] = {2, 6, 4, 8};
  int32x4_t expected1 = vld1q_s32 (exp1);
  int32x4_t expected2 = vld1q_s32 (exp2);

  for (i = 0; i < 4; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
