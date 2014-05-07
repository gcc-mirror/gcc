extern void abort (void);

poly16x8x2_t
test_vtrnqp16 (poly16x8_t _a, poly16x8_t _b)
{
  return vtrnq_p16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  poly16_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  poly16_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  poly16x8x2_t result = test_vtrnqp16 (vld1q_p16 (first), vld1q_p16 (second));
  poly16x8_t res1 = result.val[0], res2 = result.val[1];
  poly16_t exp1[] = {1, 9, 3, 11, 5, 13, 7, 15};
  poly16_t exp2[] = {2, 10, 4, 12, 6, 14, 8, 16};
  poly16x8_t expected1 = vld1q_p16 (exp1);
  poly16x8_t expected2 = vld1q_p16 (exp2);

  for (i = 0; i < 8; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
