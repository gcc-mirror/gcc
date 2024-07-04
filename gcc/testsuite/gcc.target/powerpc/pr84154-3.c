/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* PR target/84154.  Make sure on ISA 3.0 we store the result of a conversion
   to char/short using an offsettable address does not generate direct moves
   for storing 8/16/32-bit integers.  */

void
double_to_uc (double x, unsigned char *p)
{
  p[3] = x;
}

void
double_to_sc (double x, signed char *p)
{
  p[3] = x;
}

void
double_to_us (double x, unsigned short *p)
{
  p[3] = x;
}

void
double_to_ss (double x, short *p)
{
  p[3] = x;
}

void
double_to_ui (double x, unsigned int *p)
{
  p[3] = x;
}

void
double_to_si (double x, int *p)
{
  p[3] = x;
}

/* { dg-final { scan-assembler-times {\maddi\M}                   6 } } */
/* { dg-final { scan-assembler-times {\mfctiwuz\M|\mxscvdpuxws\M} 3 } } */
/* { dg-final { scan-assembler-times {\mfctiwz\M|\mxscvdpsxws\M}  3 } } */
/* { dg-final { scan-assembler-times {\mstfiwx\M|\mstxsiwx\M}     2 } } */
/* { dg-final { scan-assembler-times {\mstxsibx\M}                2 } } */
/* { dg-final { scan-assembler-times {\mstxsihx\M}                2 } } */
/* { dg-final { scan-assembler-not   {\mlbz\M}                      } } */
/* { dg-final { scan-assembler-not   {\mlhz\M}                      } } */
/* { dg-final { scan-assembler-not   {\mlha\M}                      } } */
/* { dg-final { scan-assembler-not   {\mmfvsrwz\M}                  } } */
/* { dg-final { scan-assembler-not   {\mmfvsrd\M}                   } } */
/* { dg-final { scan-assembler-not   {\mstw\M}                      } } */
/* { dg-final { scan-assembler-not   {\mstb\M}                      } } */
/* { dg-final { scan-assembler-not   {\msth\M}                      } } */
