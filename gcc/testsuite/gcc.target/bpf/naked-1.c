/* Verify that __attribute__((naked)) is accepted and
   produces a naked function.  Also, the compiler must not
   warn for the lack of return statement.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Wreturn-type" } */

int __attribute__((naked)) foo()
{
  __asm__ volatile ("@ naked");
}
/* { dg-final { scan-assembler "\t@ naked" } } */
