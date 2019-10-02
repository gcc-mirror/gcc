/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#include <stdint.h>

#define SIGNED(S) int##S##_t

#define DIV(x,y) ((x)/(y))
#define MOD(x,y) ((x)%(y))

#define TEMPLATE(OP,SIZE)						\
void __attribute__ ((noinline, noclone))				\
f_##OP##_##SIZE (SIGNED(SIZE) *restrict a, SIGNED(SIZE) *restrict b,	\
		 __INTPTR_TYPE__ n)					\
{									\
  for (__INTPTR_TYPE__ i = 0; i < n; ++i)				\
    a[i] = OP (b[i], ((SIGNED(SIZE))1 << ((SIZE)/2+1)));		\
}
#define DIVMOD(SIZE)	\
TEMPLATE (DIV,SIZE);	\
TEMPLATE (MOD,SIZE);

DIVMOD (8);
DIVMOD (16);
DIVMOD (32);
DIVMOD (64);

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 8 "vect" } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+, z[0-9]+\n} 4 } } */

/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.b, p[0-9]+/m, z[0-9]+\.b, #5\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */

/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.h, p[0-9]+/m, z[0-9]+\.h, #9\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h, z[0-9]+\.h, #9\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */

/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.s, p[0-9]+/m, z[0-9]+\.s, #17\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.s, z[0-9]+\.s, #17\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.d, p[0-9]+/m, z[0-9]+\.d, #33\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.d, z[0-9]+\.d, #33\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-not {\tasr\t%} } } */
/* { dg-final { scan-assembler-not {\tlsr\t%} } } */
/* { dg-final { scan-assembler-not {\tcmplt\t%} } } */
/* { dg-final { scan-assembler-not {\tand\t%} } } */
