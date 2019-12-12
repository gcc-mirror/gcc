int
main (int argc, char **argv)
{
  int c;
  unsigned char out[][1] = { {71}, {71}, {71} };

  for (int i = 0; i < 3; i++)
    if (!out[i][0])
      __builtin_abort ();
  return 0;
}
