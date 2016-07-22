/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

#ifndef SIZE
#define SIZE 1024
#endif

#ifndef ALIGN
#define ALIGN 32
#endif

#define ALIGN_ATTR __attribute__((__aligned__(ALIGN)))

#define DO_BUILTIN(PREFIX, TYPE, CTZ)					\
TYPE PREFIX ## _a[SIZE] ALIGN_ATTR;					\
TYPE PREFIX ## _b[SIZE] ALIGN_ATTR;					\
									\
void									\
PREFIX ## _ctz (void)							\
{									\
  unsigned long i;							\
									\
  for (i = 0; i < SIZE; i++)						\
    PREFIX ## _a[i] = CTZ (PREFIX ## _b[i]);				\
}

#if !defined(DO_LONG_LONG) && !defined(DO_LONG) && !defined(DO_INT) && !defined(DO_SHORT) && !defined(DO_CHAR)
#define DO_INT 1
#endif

#if DO_LONG_LONG
/* At the moment, only int is auto vectorized.  */
DO_BUILTIN (sll, long long,		__builtin_ctzll)
DO_BUILTIN (ull, unsigned long long,	__builtin_ctzll)
#endif

#if defined(_ARCH_PPC64) && DO_LONG
DO_BUILTIN (sl,  long,			__builtin_ctzl)
DO_BUILTIN (ul,  unsigned long,		__builtin_ctzl)
#endif

#if DO_INT
DO_BUILTIN (si,  int,			__builtin_ctz)
DO_BUILTIN (ui,  unsigned int,		__builtin_ctz)
#endif

#if DO_SHORT
DO_BUILTIN (ss,  short,			__builtin_ctz)
DO_BUILTIN (us,  unsigned short,	__builtin_ctz)
#endif

#if DO_CHAR
DO_BUILTIN (sc,  signed char,		__builtin_ctz)
DO_BUILTIN (uc,  unsigned char,		__builtin_ctz)
#endif

/* { dg-final { scan-assembler-times "vctzw" 2 } } */
/* { dg-final { scan-assembler-not "cnttzd" } } */
/* { dg-final { scan-assembler-not "cnttzw" } } */
