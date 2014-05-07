extern void abort (void);

poly8x8_t
test_vrev16p8 (poly8x8_t _arg)
{
  return vrev16_p8 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  poly8x8_t inorder = {1, 2, 3, 4, 5, 6, 7, 8};
  poly8x8_t reversed = test_vrev16p8 (inorder);
  poly8x8_t expected = {2, 1, 4, 3, 6, 5, 8, 7};

  for (i = 0; i < 8; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

