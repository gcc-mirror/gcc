extern void abort (void);

poly16x4x2_t
test_vtrnp16 (poly16x4_t _a, poly16x4_t _b)
{
  return vtrn_p16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  poly16_t first[] = {1, 2, 3, 4};
  poly16_t second[] = {5, 6, 7, 8};
  poly16x4x2_t result = test_vtrnp16 (vld1_p16 (first), vld1_p16 (second));
  poly16x4_t res1 = result.val[0], res2 = result.val[1];
  poly16_t exp1[] = {1, 5, 3, 7};
  poly16_t exp2[] = {2, 6, 4, 8};
  poly16x4_t expected1 = vld1_p16 (exp1);
  poly16x4_t expected2 = vld1_p16 (exp2);

  for (i = 0; i < 4; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
