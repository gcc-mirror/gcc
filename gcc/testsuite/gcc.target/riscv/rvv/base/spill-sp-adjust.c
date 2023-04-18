/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "spill-1.c"

void
spill_sp_adjust (int8_t *v)
{
  vint8mf8_t v1 = *(vint8mf8_t*)v; 
}

/* Make sure we do not have a useless SP adjustment.  */
/* { dg-final { scan-assembler-not "addi\tsp,sp,0" } } */
