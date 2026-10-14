/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvfqwdota8f -mabi=lp64d -O2" } */

#include "riscv_vector.h"

/* The disabled mf8 modes must produce ABI diagnostics instead of an ICE.  */
void
arg_e4 (vfloat8e4m3mf8_t x) /* { dg-error {argument type[^\n]*64[^\n]*but[^\n]*32} } */
{
}

void
arg_e5 (vfloat8e5m2mf8_t x) /* { dg-error {argument type[^\n]*64[^\n]*but[^\n]*32} } */
{
}

vfloat8e4m3mf8_t
ret_e4 (const vfloat8e4m3mf8_t *x) /* { dg-error {return type[^\n]*64[^\n]*but[^\n]*32} } */
{
  return *x;
}

vfloat8e5m2mf8_t
ret_e5 (const vfloat8e5m2mf8_t *x) /* { dg-error {return type[^\n]*64[^\n]*but[^\n]*32} } */
{
  return *x;
}
