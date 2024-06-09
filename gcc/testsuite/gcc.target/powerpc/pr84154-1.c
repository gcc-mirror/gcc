/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

/* PR target/84154.  Make sure conversion to char/short does not generate a
   store and a load on ISA 2.07 and newer systems.  */

unsigned char
double_to_uc (double x)
{
  return x;
}

signed char
double_to_sc (double x)
{
  return x;
}

unsigned short
double_to_us (double x)
{
  return x;
}

short
double_to_ss (double x)
{
  return x;
}

unsigned int
double_to_ui (double x)
{
  return x;
}

int
double_to_si (double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\mextsb\M}                  1 } } */
/* { dg-final { scan-assembler-times {\mextsh\M}                  1 } } */
/* { dg-final { scan-assembler-times {\mfctiwuz\M|\mxscvdpuxws\M} 3 } } */
/* { dg-final { scan-assembler-times {\mfctiwz\M|\mxscvdpsxws\M}  3 } } */
/* { dg-final { scan-assembler-times {\mmfvsrwz\M}                6 } } */
/* { dg-final { scan-assembler-times {\mrlwinm\M}                 2 } } */
/* { dg-final { scan-assembler-not   {\mlbz\M}                      } } */
/* { dg-final { scan-assembler-not   {\mlhz\M}                      } } */
/* { dg-final { scan-assembler-not   {\mlha\M}                      } } */
/* { dg-final { scan-assembler-not   {\mmfvsrd\M}                   } } */
/* { dg-final { scan-assembler-not   {\mstw\M}                      } } */
