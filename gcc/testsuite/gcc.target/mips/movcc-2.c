/* { dg-do compile } */
/* { dg-mips-options "-O2 -mips4" } */
/* { dg-final { scan-assembler "movz" } } */
/* { dg-final { scan-assembler "movn" } } */

void ext_long (long);

NOMIPS16 long
sub4 (long i, long j, long k)
{
  ext_long (k ? i : j);
}

NOMIPS16 long
sub5 (long i, long j, int k)
{
  ext_long (!k ? i : j);
}
