/* { dg-do compile } */
/* { dg-options "-O isa_rev>=2 -mgp32" } */
/* { dg-final { scan-assembler "\tins\t" } } */

struct
{
  unsigned int i : 2;
  unsigned int j : 3;
  unsigned int k : 4;
} s;

NOMIPS16 void
foo (void)
{
  s.j = 1;
}
