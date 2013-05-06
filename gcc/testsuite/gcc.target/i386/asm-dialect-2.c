/* { dg-do compile } */
/* { dg-options "-masm=att" } */
/* { dg-final { scan-assembler "%{a}|" } } */

int a, b;

void f()
{
  /* Check for escaped curly braces support.  */
  asm volatile ("{%%%{a%}%||%%%}b}" : :);
}
