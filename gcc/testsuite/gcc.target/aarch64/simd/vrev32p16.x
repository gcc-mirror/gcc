extern void abort (void);

poly16x4_t
test_vrev32p16 (poly16x4_t _arg)
{
  return vrev32_p16 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  poly16x4_t inorder = {1, 2, 3, 4};
  poly16x4_t reversed = test_vrev32p16 (inorder);
  poly16x4_t expected = {2, 1, 4, 3};

  for (i = 0; i < 4; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

