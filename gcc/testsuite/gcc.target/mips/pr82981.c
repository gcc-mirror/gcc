/* PR target/82981 */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O2" } } */
/* { dg-options "-march=mips64r6 -mabi=64 -mexplicit-relocs" } */

unsigned long
func (unsigned long a, unsigned long b)
{
  return a > (~0UL) / b;
}

/* { dg-final { scan-assembler-not "__multi3" } } */
/* { dg-final { scan-assembler "\tdmuhu" } } */
