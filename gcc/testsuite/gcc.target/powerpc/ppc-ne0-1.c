/* PR target/51274 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {\mcmp[wd]i\M} 1 { target {   has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\msetbcr\M}   1 { target {   has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\maddic\M}    1 { target { ! has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\msubfe\M}    1 { target { ! has_arch_pwr10 } } } } */

long ne0(long a)
{
  return a != 0;
}
