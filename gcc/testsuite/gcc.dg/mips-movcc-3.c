/* { dg-do compile { target "mips*-*-*" } } */
/* { dg-options "-O2 -mips4" } */
/* { dg-final { scan-assembler "movz.s" } } */
/* { dg-final { scan-assembler "movn.s" } } */
/* { dg-final { scan-assembler "movt.s" } } */
/* { dg-final { scan-assembler "movz.d" } } */
/* { dg-final { scan-assembler "movn.d" } } */
/* { dg-final { scan-assembler "movf.d" } } */

#if __mips < 4 || __mips_soft_float
asm ("# movz.s movn.s movt.s movz.d movn.d movf.d");
#else
void ext_float (float);
void ext_double (double);

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
#endif
