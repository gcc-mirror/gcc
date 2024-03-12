/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long long v2di __attribute__((vector_size (16)));
typedef int v2si __attribute__((vector_size (8)));

#define TESTV2DI(lab, idx)			\
  void						\
  stpv2di_##lab (v2di *x, long long a)		\
  {						\
    v2di tmp = {a, a};				\
    x[idx] = tmp;				\
  }


#define TESTV2SI(lab, idx)			\
  void						\
  stpv2si_##lab (v2si *x, int a)		\
  {						\
    v2si tmp = {a, a};				\
    x[idx] = tmp;				\
  }						\

/* Core test, no imm assembler offset:  */

TESTV2SI(0, 0)
TESTV2DI(0, 0)
/* { dg-final { scan-assembler {\s+stp\t(w[0-9]+), \1, \[x[0-9]+\]} } } */
/* { dg-final { scan-assembler {\s+stp\t(x[0-9]+), \1, \[x[0-9]+\]} } } */

/* Lower offset bounds:  */

/* Vaid offsets:  */
TESTV2SI(1, -32)
TESTV2DI(1, -32)
/* { dg-final { scan-assembler {\s+stp\t(w[0-9]+), \1, \[x[0-9]+, -256\]} } } */
/* { dg-final { scan-assembler {\s+stp\t(x[0-9]+), \1, \[x[0-9]+, -512\]} } } */
/* Invalid offsets:  */
TESTV2SI(2, -33)
TESTV2DI(2, -33)
/* { dg-final { scan-assembler-not {\s+stp\t(w[0-9]+), \1, \[x[0-9]+, -264\]} } } */
/* { dg-final { scan-assembler-not {\s+stp\t(x[0-9]+), \1, \[x[0-9]+, -528\]} } } */

/* Upper offset bounds:   */

/* Valid offsets:  */
TESTV2SI(3, 31)
TESTV2DI(3, 31)
/* { dg-final { scan-assembler {\s+stp\t(w[0-9]+), \1, \[x[0-9]+, 248\]} } } */
/* { dg-final { scan-assembler {\s+stp\t(x[0-9]+), \1, \[x[0-9]+, 496\]} } } */
/* Invalid offsets:  */
TESTV2SI(4, 32)
TESTV2DI(4, 32)
/* { dg-final { scan-assembler-not {\s+stp\t(w[0-9]+), \1, \[x[0-9]+, 256\]} } } */
/* { dg-final { scan-assembler-not {\s+stp\t(x[0-9]+), \1, \[x[0-9]+, 512\]} } } */


