/* { dg-do compile } */
/* { dg-options "-O0 -march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

void
fun (vint32m1x3_t a) { } /* { dg-warning "the vector type" } */

void
bar ()
{
  vint32m1x3_t a;
  fun (a);
}
