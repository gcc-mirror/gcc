extern void abort (void);

int16x8_t
test_vrev64qs16 (int16x8_t _arg)
{
  return vrev64q_s16 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  int16x8_t inorder = {1, 2, 3, 4, 5, 6, 7, 8};
  int16x8_t reversed = test_vrev64qs16 (inorder);
  int16x8_t expected = {4, 3, 2, 1, 8, 7, 6, 5};

  for (i = 0; i < 8; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

