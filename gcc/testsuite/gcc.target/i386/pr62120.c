/* { dg-do compile } */
/* { dg-options "-mno-sse" } */

void foo ()
{
  register int zmm_var asm ("ymm9");/* { dg-error "invalid register name" } */
  register int zmm_var2 asm ("23");/* { dg-error "invalid register name" } */
}
