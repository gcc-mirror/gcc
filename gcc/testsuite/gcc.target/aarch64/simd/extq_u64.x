extern void abort (void);

uint64x2_t
test_vextq_u64_1 (uint64x2_t a, uint64x2_t b)
{
  return vextq_u64 (a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  uint64_t arr1[] = {0, 1};
  uint64x2_t in1 = vld1q_u64 (arr1);
  uint64_t arr2[] = {2, 3};
  uint64x2_t in2 = vld1q_u64 (arr2);
  uint64_t exp[2];
  uint64x2_t expected;
  uint64x2_t actual = test_vextq_u64_1 (in1, in2);

  for (i = 0; i < 2; i++)
    exp[i] = i + 1;
  expected = vld1q_u64 (exp);
  for (i = 0; i < 2; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

