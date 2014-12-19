/* { dg-do compile } */
/* { dg-options "(HAS_MOVN)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tmovz\t" } } */
/* { dg-final { scan-assembler "\tmovn\t" } } */

void ext_int (int);

NOMIPS16 int
sub1 (int i, int j, int k)
{
  ext_int (k ? i : j);
}

NOMIPS16 int
sub2 (int i, int j, long l)
{
  ext_int (!l ? i : j);
}
