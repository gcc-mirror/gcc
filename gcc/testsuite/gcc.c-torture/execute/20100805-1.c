unsigned int foo (unsigned int a, unsigned int b)
{
  unsigned i;
  a = a & 1;
  for (i = 0; i < b; ++i)
    a = a << 1 | a >> (sizeof (unsigned int) * 8 - 1);
  return a;
}
extern void abort (void);
int main()
{
  if (foo (1, sizeof (unsigned int) * 8 + 1) != 2)
    abort ();
  return 0;
}
