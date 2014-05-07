extern void abort (void);

uint16x4x2_t
test_vtrnu16 (uint16x4_t _a, uint16x4_t _b)
{
  return vtrn_u16 (_a, _b);
}

int
main (int argc, char **argv)
{
  int i;
  uint16_t first[] = {1, 2, 3, 4};
  uint16_t second[] = {5, 6, 7, 8};
  uint16x4x2_t result = test_vtrnu16 (vld1_u16 (first), vld1_u16 (second));
  uint16x4_t res1 = result.val[0], res2 = result.val[1];
  uint16_t exp1[] = {1, 5, 3, 7};
  uint16_t exp2[] = {2, 6, 4, 8};
  uint16x4_t expected1 = vld1_u16 (exp1);
  uint16x4_t expected2 = vld1_u16 (exp2);

  for (i = 0; i < 4; i++)
    if ((res1[i] != expected1[i]) || (res2[i] != expected2[i]))
      abort ();

  return 0;
}
