/* { dg-do compile } */
/* { dg-options "-ffast-math -mno-madd4 (HAS_MADD4) -mhard-float" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tmadd.s\t" } } */

#ifndef __mips_no_madd4
#error missing definition of __mips_no_madd4
#endif

NOMIPS16 float
madd4 (float f, float g, float h)
{
  return (f * g) + h;
}
