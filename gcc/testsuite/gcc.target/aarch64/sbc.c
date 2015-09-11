/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

extern void abort (void);

typedef unsigned int u32int;
typedef unsigned long long u64int;

u32int
test_si (u32int w1, u32int w2, u32int w3, u32int w4)
{
  u32int w0;
  /* { dg-final { scan-assembler "sbc\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+\n" } } */
  w0 = w1 - w2 - (w3 < w4);
  return w0;
}

u64int
test_di (u64int x1, u64int x2, u64int x3, u64int x4)
{
  u64int x0;
  /* { dg-final { scan-assembler "sbc\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+\n" } } */
  x0 = x1 - x2 - (x3 < x4);
  return x0;
}

int
main ()
{
  u32int x;
  u64int y;
  x = test_si (7, 8, 12, 15);
  if (x != -2)
    abort();
  y = test_di (0x987654321ll, 0x123456789ll, 0x345345345ll, 0x123123123ll);
  if (y != 0x8641fdb98ll)
    abort();
  return 0;
}

