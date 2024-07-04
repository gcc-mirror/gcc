/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -fcaller-saves" } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */
/* { dg-final { scan-assembler-not "lxvw4x" } } */
/* { dg-final { scan-assembler-not "lvx" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvw4x" } } */
/* { dg-final { scan-assembler-not "stvx" } } */

/* Insure caller save on long double does not use VSX instructions.  */

#if defined(__LONG_DOUBLE_IEEE128__)
/* If long double is IEEE 128-bit, we need to use the __ibm128 type instead of
   long double.  We can't use __ibm128 on systems that don't support IEEE
   128-bit floating point, because the type is not enabled on those
   systems.  */
#define LDOUBLE __ibm128

#elif defined(__LONG_DOUBLE_IBM128__)
#define LDOUBLE long double

#else
#error "long double must be either IBM 128-bit or IEEE 128-bit"
#endif

extern LDOUBLE modify (LDOUBLE);

void
sum (LDOUBLE *ptr, LDOUBLE value, unsigned long n)
{
  unsigned long i;

  for (i = 0; i < n; i++)
    ptr[i] += modify (value);
}
