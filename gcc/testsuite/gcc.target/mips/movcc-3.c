/* { dg-do compile } */
/* { dg-mips-options "-O2 -mips4 -mhard-float" } */
/* { dg-final { scan-assembler "movt" } } */
/* { dg-final { scan-assembler "movf" } } */
/* { dg-final { scan-assembler "movz.s" } } */
/* { dg-final { scan-assembler "movn.s" } } */
/* { dg-final { scan-assembler "movt.s" } } */
/* { dg-final { scan-assembler "movz.d" } } */
/* { dg-final { scan-assembler "movn.d" } } */
/* { dg-final { scan-assembler "movf.d" } } */

void ext_int (int);
void ext_long (long);
void ext_float (float);
void ext_double (double);

int
sub3 (int i, int j, float f)
{
  ext_int (f ? i : j);
}

long
sub6 (long i, long j, float f)
{
  ext_long (!f ? i : j);
}

float
sub7 (float f, float g, int i)
{
  ext_float (i ? f : g);
}

float
sub8 (float f, float g, long l)
{
  ext_float (!l ? f : g);
}

float
sub9 (float f, float g, float h)
{
  ext_float (h ? f : g);
}

double
suba (double f, double g, int i)
{
  ext_double (i ? f : g);
}

double
subb (double f, double g, long l)
{
  ext_double (!l ? f : g);
}

double
subc (double f, double g, double h)
{
  ext_double (!h ? f : g);
}
