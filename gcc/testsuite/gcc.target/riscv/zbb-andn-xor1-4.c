/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbb -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* RV32 version of zbb-andn-xor1-2.c.  The other operand is not known to be
   0 or 1, so the xori cannot be treated as a NOT: (x ^ 1) & b clears bit 0
   of b when x is 1, whereas ~x & b would keep the upper bits of b.  */

int
f (int a, int c)
{
  return (c ^ 1) & a;
}

/* { dg-final { scan-assembler-not "\tandn\t" } } */
/* { dg-final { scan-assembler "\txori\t" } } */
