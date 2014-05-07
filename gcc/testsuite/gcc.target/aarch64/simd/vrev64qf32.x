extern void abort (void);

float32x4_t
test_vrev64qf32 (float32x4_t _arg)
{
  return vrev64q_f32 (_arg);
}

int
main (int argc, char **argv)
{
  int i;
  float32x4_t inorder = {1, 2, 3, 4};
  float32x4_t reversed = test_vrev64qf32 (inorder);
  float32x4_t expected = {2, 1, 4, 3};

  for (i = 0; i < 4; i++)
    if (reversed[i] != expected[i])
      abort ();
  return 0;
}

