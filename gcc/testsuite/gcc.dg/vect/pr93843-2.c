char in[2] = {2, 2};
short out[2] = {};

int
main()
{
  for (int i = 0; i < 2; ++i)
    out[i] = in[i];
  asm("":::"memory");
  if (out[0] != 2) __builtin_abort();
}
