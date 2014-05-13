extern void abort (void);

uint8x16_t
test_vrev16qu8 (uint8x16_t _arg)
{
  return vrev16q_u8 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  uint8x16_t inorder = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
  uint8x16_t reversed = test_vrev16qu8 (inorder);
  uint8x16_t expected = {2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11, 14, 13, 16, 15};

  for (i = 0; i < 16; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

