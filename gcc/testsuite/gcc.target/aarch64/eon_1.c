/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "\tf?mov\t" } } */

typedef long long int64_t;
typedef int64_t int64x1_t __attribute__ ((__vector_size__ (8)));

/* { dg-final { scan-assembler-times "\\teon\\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" 1 } } */

int64_t
test_eon (int64_t a, int64_t b)
{
  return a ^ ~b;
}

/* { dg-final { scan-assembler-times "\\tmvn\\tx\[0-9\]+, x\[0-9\]+" 1 } } */
int64_t
test_not (int64_t a)
{
  return ~a;
}

/* There is no eon for SIMD regs; we prefer eor+mvn to mov+mov+eon+mov.  */

/* { dg-final { scan-assembler-times "\\teor\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b, v\[0-9\]+\.8b" 1 } } */
/* { dg-final { scan-assembler-times "\\tmvn\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b" 2 } } */
int64x1_t
test_vec_eon (int64x1_t a, int64x1_t b)
{
  return a ^ ~b;
}

int64x1_t
test_vec_not (int64x1_t a)
{
  return ~a;
}

