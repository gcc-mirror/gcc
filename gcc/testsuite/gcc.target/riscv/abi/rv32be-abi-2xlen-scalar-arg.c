/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32 -mbig-endian -fno-pie -no-pie" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

typedef unsigned int u32;
typedef unsigned long long u64;

extern void named_scalar (u64);
extern void named_even_pair (u32, u32, u64);

void
call_named (u32 hi, u32 lo)
{
  u64 x = ((u64) hi << 32) | lo;
  named_scalar (x);
}

void
call_named_even_pair (u32 hi, u32 lo)
{
  u64 x = ((u64) hi << 32) | lo;
  named_even_pair (0, 1, x);
}

volatile u32 sink_words[2];
volatile u32 sink_even_pair[2];

void
consume_named (u64 x)
{
  sink_words[0] = (u32) x;
  sink_words[1] = (u32) (x >> 32);
}

void
consume_named_even_pair (u32 pad0, u32 pad1,
			 u64 x)
{
  sink_even_pair[0] = (u32) x;
  sink_even_pair[1] = (u32) (x >> 32);
}

/* Named 2 * XLEN scalars use significance ordering: a0 is the low word,
   a1 is the high word, even for big-endian.  */
/* { dg-final { scan-assembler {\mmv[ \t]+a0,a1} } } */
/* { dg-final { scan-assembler {\msw[ \t]+a0,(sink_words,|0\()} } } */
/* { dg-final { scan-assembler {\msw[ \t]+a1,(sink_words\+4|4\()} } } */
/* { dg-final { scan-assembler-not {\msw[ \t]+a1,(sink_words,|0\()} } } */

/* The same rule applies directly to even/odd register pairs used by P/Zilsd:
   a2 is the low word and a3 is the high word.  */
/* { dg-final { scan-assembler {\msw[ \t]+a2,(sink_even_pair,|0\()} } } */
/* { dg-final { scan-assembler {\msw[ \t]+a3,(sink_even_pair\+4|4\()} } } */
/* { dg-final { scan-assembler-not {\msw[ \t]+a3,(sink_even_pair,|0\()} } } */
