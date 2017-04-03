/* { dg-do compile } */
/* { dg-options "-mno-mips16 -mfp64 -mhard-float -mmsa" } */
/* { dg-skip-if "code quality test" { *-*-* }  { "-O0" } { "" } } */

typedef long long v2i64 __attribute__ ((vector_size(16)));

/* Test MSA AND.d optimization: generate BCLRI.d instead, for immediate const
   vector operand with only one bit clear.  */

void
and_d_msa (v2i64 *vx, v2i64 *vy)
{
  v2i64 and_vec = {0x7FFFFFFFFFFFFFFF, 0x7FFFFFFFFFFFFFFF};
  *vy = (*vx) & and_vec;
}
/* { dg-final { scan-assembler "bclri.d" } }  */
