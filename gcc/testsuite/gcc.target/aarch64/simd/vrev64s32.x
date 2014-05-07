extern void abort (void);

int32x2_t
test_vrev64s32 (int32x2_t _arg)
{
  return vrev64_s32 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  int32x2_t inorder = {1, 2};
  int32x2_t reversed = test_vrev64s32 (inorder);
  int32x2_t expected = {2, 1};

  for (i = 0; i < 2; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

