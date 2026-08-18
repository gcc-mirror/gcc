/* { dg-do run { target rv64 } } */
/* { dg-require-effective-target riscv_v } */
/* { dg-require-effective-target rvv_zvl128b_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */

typedef unsigned short u16 __attribute__ ((vector_size (4)));

unsigned long long g;

void __attribute__ ((noinline))
f1 (unsigned long long a3)
{
  unsigned long long v15
    = __builtin_bswap64 ((long long) 10398105857157080808ull
			 / (long long) a3);
  u16 bc13 = (u16) { 29637 };
  if (0 >= bc13[(unsigned int) v15])
    __builtin_abort ();
}

u16 __attribute__ ((noinline))
f2 (u16 in)
{
  in[(unsigned int) g] = 123;
  return in;
}

int
main (void)
{
  f1 (17752357569705450221ull);
  g = 0x0b00000000000000ull;
  u16 r = f2 ((u16) { 1, 2 });
  if (r[0] != 123 || r[1] != 2)
    __builtin_abort ();
  return 0;
}
