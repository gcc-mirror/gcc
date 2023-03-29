/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */

struct bar
{
  unsigned long a:5;
  unsigned long b:26;
  unsigned long c:22;
};

unsigned long
foo (struct bar *s)
{
  return s->b;
}

/* { dg-final { scan-assembler "th.extu\t" } } */
/* { dg-final { scan-assembler-not "andi" } } */
/* { dg-final { scan-assembler-not "slli" } } */
/* { dg-final { scan-assembler-not "srli" } } */
