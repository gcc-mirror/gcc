/* { dg-do run } */
extern void abort (void);
extern void exit (int);

volatile unsigned long long x[2];

int main ()
{
  volatile char *addr = (volatile char *) &x[0];

  x[0] = ~0ULL;
  x[1] = ~0ULL;
  __builtin_write64 (addr, 0x1122334455667788ULL);
  __builtin_write32 (addr + 8, 0x12345678);
  __builtin_write16 (addr + 12, 0xaabb);
  __builtin_write8 (addr + 14, 0xcc);

  if (x[0] != 0x1122334455667788ULL
      || x[1] != 0x12345678aabbccffULL
      || __builtin_read8 (addr) != 0x11
      || __builtin_read16 (addr + 2) != 0x3344
      || __builtin_read32 (addr + 4) != 0x55667788
      || __builtin_read64 (addr + 8) != 0x12345678aabbccffULL)
    abort ();

  __builtin_write64 (addr, 0);
  __builtin_write32 (addr + 8, 0);
  __builtin_write16 (addr + 12, 0);
  __builtin_write8 (addr + 14, 0);
  if (x[0] != 0 || x[1] != 0xff)
    abort ();

  exit (0);
}
