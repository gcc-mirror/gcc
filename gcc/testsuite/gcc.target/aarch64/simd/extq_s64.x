extern void abort (void);

int64x2_t
test_vextq_s64_1 (int64x2_t a, int64x2_t b)
{
  return vextq_s64 (a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  int64_t arr1[] = {0, 1};
  int64x2_t in1 = vld1q_s64 (arr1);
  int64_t arr2[] = {2, 3};
  int64x2_t in2 = vld1q_s64 (arr2);
  int64_t exp[2];
  int64x2_t expected;
  int64x2_t actual = test_vextq_s64_1 (in1, in2);

  for (i = 0; i < 2; i++)
    exp[i] = i + 1;
  expected = vld1q_s64 (exp);
  for (i = 0; i < 2; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

