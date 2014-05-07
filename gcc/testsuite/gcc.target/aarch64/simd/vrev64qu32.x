extern void abort (void);

uint32x4_t
test_vrev64qu32 (uint32x4_t _arg)
{
  return vrev64q_u32 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  uint32x4_t inorder = {1, 2, 3, 4};
  uint32x4_t reversed = test_vrev64qu32 (inorder);
  uint32x4_t expected = {2, 1, 4, 3};

  for (i = 0; i < 4; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

