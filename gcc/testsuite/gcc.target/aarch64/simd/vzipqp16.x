extern void abort (void);

poly16x8x2_t
test_vzipqp16 (poly16x8_t _a, poly16x8_t _b)
{
  return vzipq_p16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  poly16_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  poly16_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  poly16x8x2_t result = test_vzipqp16 (vld1q_p16 (first), vld1q_p16 (second));
  poly16x8_t res1 = result.val[0], res2 = result.val[1];
  poly16_t exp1[] = {1, 9, 2, 10, 3, 11, 4, 12};
  poly16_t exp2[] = {5, 13, 6, 14, 7, 15, 8, 16};
  poly16x8_t expected1 = vld1q_p16 (exp1);
  poly16x8_t expected2 = vld1q_p16 (exp2);

  for (i = 0; i < 8; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
