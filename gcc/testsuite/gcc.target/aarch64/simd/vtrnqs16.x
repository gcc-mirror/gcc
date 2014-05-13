extern void abort (void);

int16x8x2_t
test_vtrnqs16 (int16x8_t _a, int16x8_t _b)
{
  return vtrnq_s16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  int16_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  int16_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  int16x8x2_t result = test_vtrnqs16 (vld1q_s16 (first), vld1q_s16 (second));
  int16x8_t res1 = result.val[0], res2 = result.val[1];
  int16_t exp1[] = {1, 9, 3, 11, 5, 13, 7, 15};
  int16_t exp2[] = {2, 10, 4, 12, 6, 14, 8, 16};
  int16x8_t expected1 = vld1q_s16 (exp1);
  int16x8_t expected2 = vld1q_s16 (exp2);

  for (i = 0; i < 8; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
