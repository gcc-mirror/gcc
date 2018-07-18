/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -flto" } */

unsigned long global = -12345;

unsigned long
__attribute__((noinline))
test(unsigned long v, unsigned long v2)
{
  unsigned long x = v % v2;

  return x;
}

int main(int argc, char **argv)
{
  unsigned long r = 0;

  for (int i = 0; i < 100; i++)
    r += test(argc, global);

  if (r != 100)
    __builtin_abort ();

  return 0;
}
