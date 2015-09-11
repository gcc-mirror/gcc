/* { dg-options "-O2 --save-temps" } */
/* { dg-do run } */

extern void abort (void);

int
test_si (int a, int b)
{
  /* { dg-final { scan-assembler "extr\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, 27\n" } } */
  return (a << 5) | ((unsigned int) b >> 27);
}

long long
test_di (long long a, long long b)
{
  /* { dg-final { scan-assembler "extr\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, 45\n" } } */
  return (a << 19) | ((unsigned long long) b >> 45);
}

int
main ()
{
  int v;
  long long w;
  v = test_si (0x00000004, 0x30000000);
  if (v != 0x00000086)
    abort();
  w = test_di (0x0001040040040004ll, 0x0070050066666666ll);
  if (w != 0x2002002000200380ll)
    abort();
  return 0;
}

