/* { dg-do run } */
/* { dg-options "-O2 -fdump-rtl-combine --save-temps" } */

/* Test that we don't generate BSL when in DImode with values in integer
   registers, and do generate it where we have values in floating-point
   registers.  This is useful, as it allows us to avoid register moves
   in the general case.

   We want:
	eor	x0, x0, x1
	and	x0, x0, x2
	eor	x0, x0, x1
	ret

   Rather than:
	fmov	d2, x0
	fmov	d0, x2
	fmov	d1, x1
	bsl	v0.8b, v2.8b, v1.8b
	fmov	x0, d0
	ret  */

extern void abort (void);

unsigned long long __attribute__ ((noinline))
foo (unsigned long long a, unsigned long long b, unsigned long long c)
{
  return ((a ^ b) & c) ^ b;
}

unsigned long long __attribute__ ((noinline))
foo2 (unsigned long long a, unsigned long long b, unsigned long long c)
{
  return ((a ^ b) & c) ^ a;
}

#define force_simd(V1)   asm volatile ("mov %d0, %1.d[0]"	\
	   : "=w"(V1)						\
	   : "w"(V1)						\
	   : /* No clobbers */);

unsigned long long __attribute__ ((noinline))
bar (unsigned long long a, unsigned long long b, unsigned long long c)
{
  force_simd (a);
  force_simd (b);
  force_simd (c);
  c = ((a ^ b) & c) ^ b;
  force_simd (c);
  return c;
}

unsigned long long __attribute__ ((noinline))
bar2 (unsigned long long a, unsigned long long b, unsigned long long c)
{
  force_simd (a);
  force_simd (b);
  force_simd (c);
  c = ((a ^ b) & c) ^ a;
  force_simd (c);
  return c;
}

int
main (int argc, char** argv)
{
  unsigned long long a = 0x0123456789abcdefULL;
  unsigned long long b = 0xfedcba9876543210ULL;
  unsigned long long c = 0xaabbccddeeff7777ULL;
  if (foo (a, b, c) != bar (a, b, c))
    abort ();
  if (foo2 (a, b, c) != bar2 (a, b, c))
    abort ();
  return 0;
}

/* 2 BSL, 6 FMOV (to floating-point registers), and 2 FMOV (to general
purpose registers) for the "bar" tests, which should still use BSL.  */
/* { dg-final { scan-assembler-times "bsl\tv\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fmov\td\[0-9\]" 6 } } */
/* { dg-final { scan-assembler-times "fmov\tx\[0-9\]" 2 } } */

/* { dg-final { scan-assembler-not "bif\tv\[0-9\]" } } */
/* { dg-final { scan-assembler-not "bit\tv\[0-9\]" } } */

/* We always match the idiom during combine.  */
/* { dg-final { scan-rtl-dump-times "aarch64_simd_bsldi_internal" 2 "combine" } } */
/* { dg-final { scan-rtl-dump-times "aarch64_simd_bsldi_alt" 2 "combine" } } */
