extern void abort (void);

int
adder (int a, int b)
{
  int result;
  __asm__ ("add %w0,%w1,%w2" : "=r"(result) : "r"(a), "r"(b) : "x30");
  return result;
}

int
main (int argc, char** argv)
{
  int i;
  int total = argc;
  for (i = 0; i < 20; i++)
    total = adder (total, i);

  if (total != (190 + argc))
    abort ();

  return 0;
}
