/* Verify that __attribute__((naked)) produces a naked function 
   that does not use bx to return. Naked functions could be used
   to implement interrupt routines and must not return using bx.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* Use more arguments than we have argument registers.  */
int __attribute__((naked)) foo(int a, int b, int c, int d, int e, int f)
{
  __asm__ volatile ("@ naked");
}
/* { dg-final { scan-assembler "\t@ naked" } } */
/* { dg-final { scan-assembler-not "\tbx\tlr" } } */
