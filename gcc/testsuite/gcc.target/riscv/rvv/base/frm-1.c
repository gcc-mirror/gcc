/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t f0 ()
{
  return FRM_RNE;
}

size_t f1 ()
{
  return FRM_RTZ;
}

size_t f2 ()
{
  return FRM_RDN;
}

size_t f3 ()
{
  return FRM_RUP;
}

size_t f4 ()
{
  return FRM_RMM;
}

/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*0} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*1} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*2} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*3} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*4} 1} } */
