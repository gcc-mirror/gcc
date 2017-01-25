/* { dg-do compile } */
/* { dg-options "-ffast-math -mmadd4 (HAS_MADD4) -mhard-float" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tmadd.s\t" } } */

#ifdef __mips_no_madd4
#error unexpected definition of __mips_no_madd4
#endif

NOMIPS16 float
madd4 (float f, float g, float h)
{
  return (f * g) + h;
}
