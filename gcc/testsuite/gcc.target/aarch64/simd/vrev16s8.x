extern void abort (void);

int8x8_t
test_vrev16s8 (int8x8_t _arg)
{
  return vrev16_s8 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  int8x8_t inorder = {1, 2, 3, 4, 5, 6, 7, 8};
  int8x8_t reversed = test_vrev16s8 (inorder);
  int8x8_t expected = {2, 1, 4, 3, 6, 5, 8, 7};

  for (i = 0; i < 8; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

