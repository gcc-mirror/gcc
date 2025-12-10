/* { dg-do compile } */
/* { dg-options "-O2 -fPIE" } */
/* Same as lea-3.c, but with -fPIE.  On ia32, that requires setting up the PIC
   register, which requires an addl instruction.  */

int m;

int foo(int y)
{
  return (m+y-1)/y;
}

/* { dg-final { scan-assembler "leal" } } */
/* { dg-final { scan-assembler-not "addl" { target lp64 } } } */
/* { dg-final { scan-assembler-times "addl" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "subl" } } */
