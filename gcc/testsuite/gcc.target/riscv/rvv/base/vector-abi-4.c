/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

typedef int v4si __attribute__ ((vector_size (16)));

v4si
fun (v4si a) {  return a; }  /* { dg-bogus "the vector type" } */

void
bar ()
{
  v4si a;
  fun (a);
}
