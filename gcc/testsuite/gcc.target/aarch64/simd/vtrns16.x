extern void abort (void);

int16x4x2_t
test_vtrns16 (int16x4_t _a, int16x4_t _b)
{
  return vtrn_s16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int16_t first[] = {1, 2, 3, 4};
  int16_t second[] = {5, 6, 7, 8};
  int16x4x2_t result = test_vtrns16 (vld1_s16 (first), vld1_s16 (second));
  int16x4_t res1 = result.val[0], res2 = result.val[1];
  int16_t exp1[] = {1, 5, 3, 7};
  int16_t exp2[] = {2, 6, 4, 8};
  int16x4_t expected1 = vld1_s16 (exp1);
  int16x4_t expected2 = vld1_s16 (exp2);

  for (i = 0; i < 4; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
