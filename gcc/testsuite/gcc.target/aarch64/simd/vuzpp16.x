extern void abort (void);

poly16x4x2_t
test_vuzpp16 (poly16x4_t _a, poly16x4_t _b)
{
  return vuzp_p16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  poly16_t first[] = {1, 2, 3, 4};
  poly16_t second[] = {5, 6, 7, 8};
  poly16x4x2_t result = test_vuzpp16 (vld1_p16 (first), vld1_p16 (second));
  poly16_t exp1[] = {1, 3, 5, 7};
  poly16_t exp2[] = {2, 4, 6, 8};
  poly16x4_t expect1 = vld1_p16 (exp1);
  poly16x4_t expect2 = vld1_p16 (exp2);

  for (i = 0; i < 4; i++)
    if ((result.val[0][i] != expect1[i]) || (result.val[1][i] != expect2[i]))
      abort ();

  return 0;
}
