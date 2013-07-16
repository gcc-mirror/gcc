/* PR rtl-optimization/56151 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int vara, varb;

void
foo (int i, int j)
{
  vara = varb | vara;
}

/* Verify the above is compiled into movl varb, %reg; orl %reg, vara instead
   of longer movl vara, %reg; orl varb, %reg; movl %reg, vara.  */
/* { dg-final { scan-assembler-not "mov\[^\n\r]*vara" { target nonpic } } } */
/* { dg-final { scan-assembler-times "mov\[^\n\r]*varb" 1 { target nonpic } } } */
/* { dg-final { scan-assembler-times "or\[^\n\r]*vara" 1 { target nonpic } } } */
