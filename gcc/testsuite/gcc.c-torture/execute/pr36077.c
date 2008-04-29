extern void abort (void);

unsigned int test (unsigned int x)
{
  return x / 0x80000001U / 0x00000002U;
}

int main()
{
  if (test(2) != 0)
    abort ();
  return 0;
}
