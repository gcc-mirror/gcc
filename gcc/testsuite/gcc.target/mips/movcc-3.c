/* { dg-do compile } */
/* { dg-options "(HAS_MOVN) -mhard-float" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tmovt\t" } } */
/* { dg-final { scan-assembler "\tmovf\t" } } */
/* { dg-final { scan-assembler "\tmovz.s\t" } } */
/* { dg-final { scan-assembler "\tmovn.s\t" } } */
/* { dg-final { scan-assembler "\tmovt.s\t" } } */
/* { dg-final { scan-assembler "\tmovz.d\t" } } */
/* { dg-final { scan-assembler "\tmovn.d\t" } } */
/* { dg-final { scan-assembler "\tmovf.d\t" } } */

void ext_int (int);
void ext_long (long);
void ext_float (float);
void ext_double (double);

NOMIPS16 int
sub3 (int i, int j, float f)
{
  ext_int (f ? i : j);
}

NOMIPS16 long
sub6 (long i, long j, float f)
{
  ext_long (!f ? i : j);
}

NOMIPS16 float
sub7 (float f, float g, int i)
{
  ext_float (i ? f : g);
}

NOMIPS16 float
sub8 (float f, float g, long l)
{
  ext_float (!l ? f : g);
}

NOMIPS16 float
sub9 (float f, float g, float h)
{
  ext_float (h ? f : g);
}

NOMIPS16 double
suba (double f, double g, int i)
{
  ext_double (i ? f : g);
}

NOMIPS16 double
subb (double f, double g, long l)
{
  ext_double (!l ? f : g);
}

NOMIPS16 double
subc (double f, double g, double h)
{
  ext_double (!h ? f : g);
}
