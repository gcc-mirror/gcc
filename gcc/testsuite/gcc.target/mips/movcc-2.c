/* { dg-do compile } */
/* { dg-options "(HAS_MOVN)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tmovz\t" } } */
/* { dg-final { scan-assembler "\tmovn\t" } } */
/* { dg-final { scan-assembler "\tmovz\t" } } */
/* { dg-final { scan-assembler "\tmovn\t" } } */

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

NOMIPS16 long
sub6 (int k)
{
  return !k ? 100 : 1000;
}

NOMIPS16 long
sub7 (int k)
{
  return !k ? 100 : 1000;
}
