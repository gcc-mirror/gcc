extern void abort (void);

poly8x8_t
test_vrev32p8 (poly8x8_t _arg)
{
  return vrev32_p8 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  poly8x8_t inorder = {1, 2, 3, 4, 5, 6, 7, 8};
  poly8x8_t reversed = test_vrev32p8 (inorder);
  poly8x8_t expected = {4, 3, 2, 1, 8, 7, 6, 5};

  for (i = 0; i < 8; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

