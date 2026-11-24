/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* The other operand is not known to be 0 or 1, so the xori cannot be
   treated as a NOT: (x ^ 1) & b clears bit 0 of b when x is 1, whereas
   ~x & b would keep the upper bits of b.  */

long
f (long a, long b, long c, long d)
{
  return (long) !(c < d) & a;
}

/* { dg-final { scan-assembler-not "\tandn\t" } } */
