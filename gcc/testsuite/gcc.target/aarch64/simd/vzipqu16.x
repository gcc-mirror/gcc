extern void abort (void);

uint16x8x2_t
test_vzipqu16 (uint16x8_t _a, uint16x8_t _b)
{
  return vzipq_u16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint16_t first[] = {1, 2, 3, 4, 5, 6, 7, 8};
  uint16_t second[] = {9, 10, 11, 12, 13, 14, 15, 16};
  uint16x8x2_t result = test_vzipqu16 (vld1q_u16 (first), vld1q_u16 (second));
  uint16x8_t res1 = result.val[0], res2 = result.val[1];
  uint16_t exp1[] = {1, 9, 2, 10, 3, 11, 4, 12};
  uint16_t exp2[] = {5, 13, 6, 14, 7, 15, 8, 16};
  uint16x8_t expected1 = vld1q_u16 (exp1);
  uint16x8_t expected2 = vld1q_u16 (exp2);

  for (i = 0; i < 8; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
