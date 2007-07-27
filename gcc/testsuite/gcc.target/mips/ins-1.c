/* { dg-do compile } */
/* { dg-mips-options "-O -march=mips32r2 -mno-mips16" } */
/* { dg-final { scan-assembler "\tins\t" } } */

struct
{
  unsigned int i : 2;
  unsigned int j : 3;
  unsigned int k : 4;
} s;

void
foo (void)
{
  s.j = 1;
}
