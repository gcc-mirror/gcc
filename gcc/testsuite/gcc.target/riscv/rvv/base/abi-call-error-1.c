/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -Wno-implicit-function-declaration" } */

#include "riscv_vector.h"

int
foo (int8_t *in)
{
  vint8m1_t a = *(vint8m1_t *)in;
  bar (a); /* { dg-error "RVV type 'vint8m1_t' cannot be passed to an unprototyped function" } */
}
