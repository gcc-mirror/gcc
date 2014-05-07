extern void abort (void);

int
main (int argc, char **argv)
{
  int i, off;
  uint64_t arr1[] = {0};
  uint64x1_t in1 = vld1_u64 (arr1);
  uint64_t arr2[] = {1};
  uint64x1_t in2 = vld1_u64 (arr2);
  uint64x1_t actual = vext_u64 (in1, in2, 0);
  if (actual != in1)
    abort ();

  return 0;
}

