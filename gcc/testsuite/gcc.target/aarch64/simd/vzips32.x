extern void abort (void);

int32x2x2_t
test_vzips32 (int32x2_t _a, int32x2_t _b)
{
  return vzip_s32 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int32_t first[] = {1, 2};
  int32_t second[] = {3, 4};
  int32x2x2_t result = test_vzips32 (vld1_s32 (first), vld1_s32 (second));
  int32x2_t res1 = result.val[0], res2 = result.val[1];
  int32_t exp1[] = {1, 3};
  int32_t exp2[] = {2, 4};
  int32x2_t expected1 = vld1_s32 (exp1);
  int32x2_t expected2 = vld1_s32 (exp2);

  for (i = 0; i < 2; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
