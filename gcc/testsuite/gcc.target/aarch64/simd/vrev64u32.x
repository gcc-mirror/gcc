extern void abort (void);

uint32x2_t
test_vrev64u32 (uint32x2_t _arg)
{
  return vrev64_u32 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  uint32x2_t inorder = {1, 2};
  uint32x2_t reversed = test_vrev64u32 (inorder);
  uint32x2_t expected = {2, 1};

  for (i = 0; i < 2; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

