extern void abort (void);

uint8x8_t
test_vrev64u8 (uint8x8_t _arg)
{
  return vrev64_u8 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  uint8x8_t inorder = {1, 2, 3, 4, 5, 6, 7, 8};
  uint8x8_t reversed = test_vrev64u8 (inorder);
  uint8x8_t expected = {8, 7, 6, 5, 4, 3, 2, 1};

  for (i = 0; i < 8; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

