/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times ".COND_FMA" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times ".COND_FNMA" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times ".COND_FMS" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times ".COND_FNMS" 3 "optimized" } } */
/* { dg-final { scan-assembler-times "vfmadd132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfmsub132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfmadd231pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmadd231pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfmsub231pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub231pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfmadd132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfmsub132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#ifndef NUM
#define NUM 800
#endif
#ifndef TYPE
#define TYPE double
#endif
#ifndef __BUILTIN_FMA
#define __BUILTIN_FMA __builtin_fma
#endif

TYPE a[NUM], b[NUM], c[NUM], d[NUM], e[NUM], j[NUM];
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) < (Y) ? (Y) : (X))

#define FMA3(OPNAME, OP1, OP2)					\
  void								\
  __attribute__ ((noipa,optimize ("O3")))			\
  foo3_##OPNAME ()						\
  {								\
    for (int i = 0; i != NUM; i++)				\
      {								\
	TYPE tmp = MAX(d[i], e[i]);				\
	if (b[i] < c[i])					\
	  a[i] = __BUILTIN_FMA (OP1 d[i], e[i], OP2 tmp);	\
	else							\
	  a[i] = tmp;						\
      }								\
  }

#define FMAZ(OPNAME, OP1, OP2)					\
  void								\
  __attribute__ ((noipa,optimize ("O3")))			\
  fooz_##OPNAME ()						\
  {								\
    for (int i = 0; i != NUM; i++)				\
      if (b[i] < c[i])						\
	a[i] = __BUILTIN_FMA (OP1 d[i], e[i], OP2 a[i]);	\
      else							\
	a[i] = .0;						\
  }

#define FMA1(OPNAME, OP1, OP2)					\
  void								\
  __attribute__ ((noipa,optimize ("O3")))			\
  foo1_##OPNAME ()						\
  {								\
    for (int i = 0; i != NUM; i++)				\
      if (b[i] < c[i])						\
	a[i] = __BUILTIN_FMA (OP1 d[i], e[i], OP2 a[i]);	\
      else							\
	a[i] = d[i];						\
  }


FMAZ (fma,, +);
FMAZ (fms,, -);
FMAZ (fnma, -, +);
FMAZ (fnms, -, -);

FMA1 (fma,, +);
FMA1 (fms,, -);
FMA1 (fnma, -, +);
FMA1 (fnms, -, -);

FMA3 (fma,, +);
FMA3 (fms,, -);
FMA3 (fnma, -, +);
FMA3 (fnms, -, -);
