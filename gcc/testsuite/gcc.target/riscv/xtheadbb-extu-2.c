/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */

struct bar
{
  unsigned long a:6;
  unsigned long b:26;
  unsigned long c:22;
};

/* We prefer andi over th.extu because it can be compressed.  */

unsigned long
foo (struct bar *s)
{
  return s->a;
}

/* { dg-final { scan-assembler-not "th.extu\t" } } */
/* { dg-final { scan-assembler "andi\t" } } */
