extern void abort (void);

float32x2_t
test_vrev64f32 (float32x2_t _arg)
{
  return vrev64_f32 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  float32x2_t inorder = {1, 2};
  float32x2_t reversed = test_vrev64f32 (inorder);
  float32x2_t expected = {2, 1};

  for (i = 0; i < 2; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

