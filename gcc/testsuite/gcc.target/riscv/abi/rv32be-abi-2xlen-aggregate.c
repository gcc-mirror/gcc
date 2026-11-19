/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32 -mbig-endian -fno-pie -no-pie" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

typedef unsigned int u32;
struct pair
{
  u32 high;
  u32 low;
};

extern void consume_pair (struct pair);

void
call_pair (u32 high, u32 low)
{
  struct pair x = { high, low };
  consume_pair (x);
}

volatile unsigned int sink_pair[2];

void
store_pair (struct pair x)
{
  sink_pair[0] = x.high;
  sink_pair[1] = x.low;
}

/* Aggregates keep memory-layout ordering: the lower-numbered register holds
   the lower-addressed XLEN chunk.  */
/* { dg-final { scan-assembler-not {\mmv[ \t]+a0,a1} } } */
/* { dg-final { scan-assembler {\msw[ \t]+a0,(sink_pair,|0\()} } } */
/* { dg-final { scan-assembler {\msw[ \t]+a1,(sink_pair\+4|4\()} } } */
