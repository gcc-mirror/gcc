/* { dg-do compile } */
/* { dg-options "-O1 -fnon-call-exceptions" } */

#include "arm_sve.h"

svint8_t
test_s8(int8_t *x)
{
  try
    {
      return svld1rq_s8 (svptrue_b8 (), &x[0]);
    }
  catch (...)
    {
      return svdup_s8 (1);
    }
}

/* { dg-final { scan-assembler "__cxa_begin_catch" } } */
