/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_arch_v7a } */
/* { dg-final { scan-assembler "cmp\tr\[0-9\]*, r\[0-9\]*, asr #31" } } */

typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
void abort (void);

SItype
__mulvsi3 (SItype a, SItype b)
{
  const DItype w = (DItype) a * (DItype) b;
  if ((SItype) (w >> (4 * 8)) != (SItype) w >> ((4 * 8) - 1))
    abort ();
  return w;
}
