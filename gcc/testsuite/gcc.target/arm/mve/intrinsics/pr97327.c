/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=hard" } { "" } } */
/* { dg-additional-options "-mcpu=cortex-m55 -mthumb -mfloat-abi=soft -mfpu=auto -Werror" } */

int main ()
{
  return 0;
}
