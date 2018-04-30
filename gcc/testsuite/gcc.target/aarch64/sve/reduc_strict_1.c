/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define NUM_ELEMS(TYPE) ((int)(5 * (256 / sizeof (TYPE)) + 3))

#define DEF_REDUC_PLUS(TYPE)			\
  TYPE __attribute__ ((noinline, noclone))	\
  reduc_plus_##TYPE (TYPE *a, TYPE *b)		\
  {						\
    TYPE r = 0, q = 3;				\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
      {						\
	r += a[i];				\
	q -= b[i];				\
      }						\
    return r * q;				\
  }

#define TEST_ALL(T) \
  T (_Float16) \
  T (float) \
  T (double)

TEST_ALL (DEF_REDUC_PLUS)

/* { dg-final { scan-assembler-times {\tfadda\th[0-9]+, p[0-7], h[0-9]+, z[0-9]+\.h} 2 } } */
/* { dg-final { scan-assembler-times {\tfadda\ts[0-9]+, p[0-7], s[0-9]+, z[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tfadda\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d} 2 } } */
