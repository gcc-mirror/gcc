/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 -mlittle-endian --save-temps" } */

typedef char vnx16qi __attribute__((vector_size(32)));
typedef struct { vnx16qi a[2]; } vnx32qi;

typedef short vnx8hi __attribute__((vector_size(32)));
typedef struct { vnx8hi a[2]; } vnx16hi;

typedef int vnx4si __attribute__((vector_size(32)));
typedef struct { vnx4si a[2]; } vnx8si;

typedef long vnx2di __attribute__((vector_size(32)));
typedef struct { vnx2di a[2]; } vnx4di;

typedef float vnx4sf __attribute__((vector_size(32)));
typedef struct { vnx4sf a[2]; } vnx8sf;

typedef double vnx2df __attribute__((vector_size(32)));
typedef struct { vnx2df a[2]; } vnx4df;

#define TEST_TYPE(TYPE, REG1, REG2)			\
  void							\
  f1_##TYPE (TYPE *a)					\
  {							\
    register TYPE x asm (#REG1) = a[0];			\
    asm volatile ("# test " #TYPE " 1 %S0" :: "w" (x));	\
    register TYPE y asm (#REG2) = x;			\
    asm volatile ("# test " #TYPE " 2 %S0, %S1, %S2"	\
		  : "=&w" (x) : "0" (x), "w" (y));	\
    a[1] = x;						\
  }							\
  /* This must compile, but we don't care how.  */	\
  void							\
  f2_##TYPE (TYPE *a)					\
  {							\
    TYPE x = a[0];					\
    x.a[0][3] = 1;					\
    x.a[1][2] = 12;					\
    asm volatile ("# %0" :: "w" (x));			\
  }							\
  void							\
  f3_##TYPE (TYPE *a, int i)				\
  {							\
    TYPE x = a[0];					\
    x.a[0][i] = 1;					\
    asm volatile ("# %0" :: "w" (x));			\
  }							\
  void							\
  f4_##TYPE (TYPE *a, int i, int j)			\
  {							\
    TYPE x = a[0];					\
    x.a[i][j] = 44;					\
    asm volatile ("# %0" :: "w" (x));			\
  }

TEST_TYPE (vnx32qi, z0, z2)
TEST_TYPE (vnx16hi, z5, z7)
TEST_TYPE (vnx8si, z10, z12)
TEST_TYPE (vnx4di, z15, z17)
TEST_TYPE (vnx8sf, z20, z23)
TEST_TYPE (vnx4df, z28, z30)

/* { dg-final { scan-assembler {\tldr\tz0, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz1, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx32qi 1 z0\n} } } */
/* { dg-final { scan-assembler {\tmov\tz2.d, z0.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz3.d, z1.d\n} } } */
/* { dg-final { scan-assembler { test vnx32qi 2 z0, z0, z2\n} } } */
/* { dg-final { scan-assembler {\tstr\tz0, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz1, \[x0, #3, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz5, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz6, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx16hi 1 z5\n} } } */
/* { dg-final { scan-assembler {\tmov\tz7.d, z5.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz8.d, z6.d\n} } } */
/* { dg-final { scan-assembler { test vnx16hi 2 z5, z5, z7\n} } } */
/* { dg-final { scan-assembler {\tstr\tz5, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz6, \[x0, #3, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz10, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz11, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx8si 1 z10\n} } } */
/* { dg-final { scan-assembler {\tmov\tz12.d, z10.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz13.d, z11.d\n} } } */
/* { dg-final { scan-assembler { test vnx8si 2 z10, z10, z12\n} } } */
/* { dg-final { scan-assembler {\tstr\tz10, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz11, \[x0, #3, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz15, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz16, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx4di 1 z15\n} } } */
/* { dg-final { scan-assembler {\tmov\tz17.d, z15.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz18.d, z16.d\n} } } */
/* { dg-final { scan-assembler { test vnx4di 2 z15, z15, z17\n} } } */
/* { dg-final { scan-assembler {\tstr\tz15, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz16, \[x0, #3, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz20, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz21, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx8sf 1 z20\n} } } */
/* { dg-final { scan-assembler {\tmov\tz23.d, z20.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz24.d, z21.d\n} } } */
/* { dg-final { scan-assembler { test vnx8sf 2 z20, z20, z23\n} } } */
/* { dg-final { scan-assembler {\tstr\tz20, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz21, \[x0, #3, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz28, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz29, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx4df 1 z28\n} } } */
/* { dg-final { scan-assembler {\tmov\tz30.d, z28.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz31.d, z29.d\n} } } */
/* { dg-final { scan-assembler { test vnx4df 2 z28, z28, z30\n} } } */
/* { dg-final { scan-assembler {\tstr\tz28, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz29, \[x0, #3, mul vl\]\n} } } */
