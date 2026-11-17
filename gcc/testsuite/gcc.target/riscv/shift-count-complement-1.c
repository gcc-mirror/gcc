/* { dg-do run { target { rv64 && riscv_b_ok } } } */
/* { dg-options "-O2 -march=rv64gc_zbs -mabi=lp64d" } */

/* The narrow count lets the 31 - y splitter run before combining the
   shifted mask with a Zbs operation.  */
#define OPERATOR(NAME, OP) \
__attribute__ ((noipa)) unsigned long \
NAME (unsigned long x, unsigned int y) \
{ \
  unsigned char n = 31 - y; \
  return x OP (unsigned long) (1U << n); \
}

OPERATOR (set, |)
OPERATOR (invert, ^)
OPERATOR (clear, & ~)

__attribute__ ((noipa)) unsigned long
mask (unsigned int y)
{
  unsigned char n = 31 - y;
  return (unsigned long) (1U << n);
}

__attribute__ ((noipa)) long
signed_mask (unsigned int y)
{
  unsigned char n = 31 - y;
  return (int) (1U << n);
}

int
main (void)
{
  unsigned long values[] = { 0, ~0UL, 0xaaaaaaaa55555555UL };

  for (unsigned int y = 0; y < 32; ++y)
    {
      unsigned long m = 0x80000000UL >> y;
      long sm = y == 0 ? -0x80000000L : (long) m;
      if (mask (y) != m || signed_mask (y) != sm)
	__builtin_abort ();

      for (unsigned int i = 0; i < 3; ++i)
	{
	  unsigned long x = values[i];
	  if (set (x, y) != (x | m)
	      || invert (x, y) != (x ^ m)
	      || clear (x, y) != (x & ~m))
	    __builtin_abort ();
	}
    }
  return 0;
}
