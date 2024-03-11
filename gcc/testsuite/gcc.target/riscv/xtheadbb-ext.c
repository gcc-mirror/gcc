/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */

struct bar
{
  long a:9;
  long b:26;
  long c:22;
};

long
foo (struct bar *s)
{
  return s->b;
}

/* { dg-final { scan-assembler "th.ext\t" } } */
/* { dg-final { scan-assembler-not {\mandi} } } */
