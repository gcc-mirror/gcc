/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_float128_sw_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -mfloat128 -mno-regnames" } */

#ifndef __FLOAT128__
#error "-mfloat128 is not supported."
#endif

#ifdef __LONG_DOUBLE_IEEE128__
#define TYPE long double
#define ONE  1.0L

#else
#define TYPE __float128
#define ONE  1.0Q
#endif

/* Test to make sure vector registers are used for passing IEEE 128-bit
   floating point values and returning them. Also make sure the 'q' suffix is
   handled.  */
TYPE one (void) { return ONE; }
void store (TYPE a, TYPE *p) { *p = a; }

/* { dg-final { scan-assembler "lxvd2x 34"  } } */
/* { dg-final { scan-assembler "stxvd2x 34" } } */
