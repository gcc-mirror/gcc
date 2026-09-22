/* { dg-do run } */
/* { dg-require-effective-target int32 } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef signed char s8;

__attribute__((noipa)) static long
sext_ref (unsigned long x, volatile int *pn)
{
  int n = *pn;
  volatile unsigned long b = 1ul << (n - 1);
  volatile unsigned long v = x & (b + b - 1);
  return (v & b) ? (long) v - (long) (b + b) : (long) v;
}

__attribute__((noipa)) static int
xor_sub (int x, volatile int *pm)
{
  return (x ^ *pm) - *pm;
}

__attribute__((noipa)) static int
xa8 (u8 x)
{
  return (int) ((x ^ 0x80u) - 0x80u);
}

__attribute__((noipa)) static int
xa16 (u16 x)
{
  return (int) ((x ^ 0x8000u) - 0x8000u);
}

/* N = 1: the narrow type is a 1-bit signed type.  */
__attribute__((noipa)) static int
xa1 (u8 c)
{
  int x = c >> 7;
  return (x ^ 1) - 1;
}

__attribute__((noipa)) static int
xa_signed (s8 x)
{
  return (int) (s8) (x ^ (s8) 0x80) - 128;
}

/* The xor is done in a signed type of precision 8 whose operand does fit
   8 bits, so only the sign of the xor constant refuses the fold.  */
__attribute__((noipa)) static int
xa_signed_ranged (int a)
{
  s8 x = (s8) (a & 0x3f);
  s8 t = x ^ (s8) 0x80;
  return (int) t - 128;
}

__attribute__((noipa)) static int
xa_unknown (int x)
{
  return (x ^ 0x80) - 0x80;
}

__attribute__((noipa)) static int
cb8 (u8 x)
{
  unsigned e = x;
  if (e & 0x80)
    e -= 0x100;
  return (int) e;
}

__attribute__((noipa)) static int
cb16 (u16 x)
{
  unsigned e = x;
  if (e & 0x8000)
    e -= 0x10000;
  return (int) e;
}

__attribute__((noipa)) static int
cb_wrong_const (u8 x)
{
  int e = x;
  if ((s8) x < 0)
    e -= 0x80;
  return e;
}

int
main (void)
{
  volatile int n8 = 8, n16 = 16, m80 = 0x80;
  int i;

  for (i = 0; i < 256; i++)
    {
      u8 x = (u8) i;
      if (xa8 (x) != sext_ref (x, &n8))
	__builtin_abort ();
      if (cb8 (x) != sext_ref (x, &n8))
	__builtin_abort ();
      if (xa1 (x) != -(x >> 7))
	__builtin_abort ();
      if (xa_signed ((s8) i) != (int) (s8) ((s8) i ^ (s8) 0x80) - 128)
	__builtin_abort ();
      if (xa_signed_ranged (i) != (i & 0x3f) - 256)
	__builtin_abort ();
      if (xa_unknown (i - 128) != xor_sub (i - 128, &m80))
	__builtin_abort ();
      if (cb_wrong_const (x) != (((s8) x < 0) ? (int) x - 0x80 : (int) x))
	__builtin_abort ();
    }
  for (i = 0; i < 65536; i++)
    {
      u16 x = (u16) i;
      if (xa16 (x) != sext_ref (x, &n16))
	__builtin_abort ();
      if (cb16 (x) != sext_ref (x, &n16))
	__builtin_abort ();
    }
  return 0;
}
