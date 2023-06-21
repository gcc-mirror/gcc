/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

vint32m1x3_t*
fun (vint32m1x3_t* a) {  return a; }  /* { dg-bogus "the vector type" } */

void
bar ()
{
  vint32m1x3_t a;
  fun (&a);
}
