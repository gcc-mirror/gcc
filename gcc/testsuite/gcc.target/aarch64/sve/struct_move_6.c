/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 -mlittle-endian --save-temps" } */

typedef char vnx16qi __attribute__((vector_size(32)));
typedef struct { vnx16qi a[4]; } vnx64qi;

typedef short vnx8hi __attribute__((vector_size(32)));
typedef struct { vnx8hi a[4]; } vnx32hi;

typedef int vnx4si __attribute__((vector_size(32)));
typedef struct { vnx4si a[4]; } vnx16si;

typedef long vnx2di __attribute__((vector_size(32)));
typedef struct { vnx2di a[4]; } vnx8di;

typedef float vnx4sf __attribute__((vector_size(32)));
typedef struct { vnx4sf a[4]; } vnx16sf;

typedef double vnx2df __attribute__((vector_size(32)));
typedef struct { vnx2df a[4]; } vnx8df;

#define TEST_TYPE(TYPE, REG1, REG2) \
  void \
  f_##TYPE (TYPE *a) \
  { \
    register TYPE x asm (#REG1) = a[0]; \
    asm volatile ("# test " #TYPE " 1 %S0" :: "w" (x)); \
    register TYPE y asm (#REG2) = x; \
    asm volatile ("# test " #TYPE " 2 %S0, %S1, %S2" \
		  : "=&w" (x) : "0" (x), "w" (y)); \
    a[1] = x; \
  }

TEST_TYPE (vnx64qi, z0, z4)
TEST_TYPE (vnx32hi, z6, z2)
TEST_TYPE (vnx16si, z12, z16)
TEST_TYPE (vnx8di, z17, z13)
TEST_TYPE (vnx16sf, z20, z16)
TEST_TYPE (vnx8df, z24, z28)

/* { dg-final { scan-assembler {\tldr\tz0, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz1, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz2, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz3, \[x0, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx64qi 1 z0\n} } } */
/* { dg-final { scan-assembler {\tmov\tz4.d, z0.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz5.d, z1.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz6.d, z2.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz7.d, z3.d\n} } } */
/* { dg-final { scan-assembler { test vnx64qi 2 z0, z0, z4\n} } } */
/* { dg-final { scan-assembler {\tstr\tz0, \[x0, #4, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz1, \[x0, #5, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz2, \[x0, #6, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz3, \[x0, #7, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz6, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz7, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz8, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz9, \[x0, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx32hi 1 z6\n} } } */
/* { dg-final { scan-assembler {\tmov\tz2.d, z6.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz3.d, z7.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz4.d, z8.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz5.d, z9.d\n} } } */
/* { dg-final { scan-assembler { test vnx32hi 2 z6, z6, z2\n} } } */
/* { dg-final { scan-assembler {\tstr\tz6, \[x0, #4, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz7, \[x0, #5, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz8, \[x0, #6, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz9, \[x0, #7, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz12, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz13, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz14, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz15, \[x0, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx16si 1 z12\n} } } */
/* { dg-final { scan-assembler {\tmov\tz16.d, z12.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz17.d, z13.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz18.d, z14.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz19.d, z15.d\n} } } */
/* { dg-final { scan-assembler { test vnx16si 2 z12, z12, z16\n} } } */
/* { dg-final { scan-assembler {\tstr\tz12, \[x0, #4, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz13, \[x0, #5, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz14, \[x0, #6, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz15, \[x0, #7, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz17, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz18, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz19, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz20, \[x0, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx8di 1 z17\n} } } */
/* { dg-final { scan-assembler {\tmov\tz13.d, z17.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz14.d, z18.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz15.d, z19.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz16.d, z20.d\n} } } */
/* { dg-final { scan-assembler { test vnx8di 2 z17, z17, z13\n} } } */
/* { dg-final { scan-assembler {\tstr\tz17, \[x0, #4, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz18, \[x0, #5, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz19, \[x0, #6, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz20, \[x0, #7, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz20, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz21, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz22, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz23, \[x0, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx16sf 1 z20\n} } } */
/* { dg-final { scan-assembler {\tmov\tz16.d, z20.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz17.d, z21.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz18.d, z22.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz19.d, z23.d\n} } } */
/* { dg-final { scan-assembler { test vnx16sf 2 z20, z20, z16\n} } } */
/* { dg-final { scan-assembler {\tstr\tz20, \[x0, #4, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz21, \[x0, #5, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz22, \[x0, #6, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz23, \[x0, #7, mul vl\]\n} } } */

/* { dg-final { scan-assembler {\tldr\tz24, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz25, \[x0, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz26, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tldr\tz27, \[x0, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler { test vnx8df 1 z24\n} } } */
/* { dg-final { scan-assembler {\tmov\tz28.d, z24.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz29.d, z25.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz30.d, z26.d\n} } } */
/* { dg-final { scan-assembler {\tmov\tz31.d, z27.d\n} } } */
/* { dg-final { scan-assembler { test vnx8df 2 z24, z24, z28\n} } } */
/* { dg-final { scan-assembler {\tstr\tz24, \[x0, #4, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz25, \[x0, #5, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz26, \[x0, #6, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tstr\tz27, \[x0, #7, mul vl\]\n} } } */
