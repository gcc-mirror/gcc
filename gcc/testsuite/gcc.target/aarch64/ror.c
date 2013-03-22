/* { dg-options "-O2 --save-temps" } */
/* { dg-do run } */

extern void abort (void);

int
test_si (int a)
{
  /* { dg-final { scan-assembler "ror\tw\[0-9\]+, w\[0-9\]+, 27\n" } } */
  return (a << 5) | ((unsigned int) a >> 27);
}

long long
test_di (long long a)
{
  /* { dg-final { scan-assembler "ror\tx\[0-9\]+, x\[0-9\]+, 45\n" } } */
  return (a << 19) | ((unsigned long long) a >> 45);
}

int
main ()
{
  int v;
  long long w;
  v = test_si (0x0203050);
  if (v != 0x4060a00)
    abort();
  w = test_di (0x0000020506010304ll);
  if (w != 0x1028300818200000ll)
    abort();
  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
