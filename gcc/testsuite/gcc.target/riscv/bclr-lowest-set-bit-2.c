/* { dg-do run } */
/* { dg-additional options "-march=rv64gc_zbb_zbs -mabi=lp64d" { target rv64 } } */
/* { dg-additional options "-march=rv32gc_zbb_zbs -mabi=ilp32" { target rv32 } } */

/* If bclr drops the sign extension, this loop never terminates.  */

extern void abort (void);

__attribute__((noipa)) unsigned
bit_walk (unsigned x)
{
  unsigned r = 0;
  unsigned guard = 0;

  for (unsigned m = x; m; m &= m - 1)
    {
      if (++guard > 32)
	abort ();
      r += (unsigned) __builtin_ctz (m);
    }
  return r;
}

int
main (void)
{
  if (bit_walk (0x80000001u) != 31)
    abort ();
  return 0;
}
