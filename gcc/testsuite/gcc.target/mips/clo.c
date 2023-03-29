/* { dg-do compile } */
/* { dg-options "(HAS_CLZ)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

NOMIPS16 unsigned int foo(unsigned int x)
{
  return  __builtin_clz (~x);
}

/* { dg-final { scan-assembler-not "\tclz\t" } } */
/* { dg-final { scan-assembler "\tclo\t" } } */
