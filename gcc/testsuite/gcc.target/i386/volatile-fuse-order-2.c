/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -masm=att" } */
/* { dg-final { check-function-bodies "ia32" "*ia32" "" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { check-function-bodies "x64" "*x64" "" { target { { ! ia32 } && { ! *-*-darwin* } } } } } */

/* Both reads of *P are in separate full expressions: the first read must
   be the one loaded into a register before the compare, which performs
   the second read.  */

/*
ia32f:
ia32...
ia32	movl	\(%eax\), %edx
ia32	cmpl	\(%eax\), %edx
ia32...
*ia32

x64f:
x64...
x64	movl	\(%(r|e)di\), %eax
x64	cmpl	\(%(r|e)di\), %eax
x64...
*x64
*/

int
f (volatile int *p)
{
  int a = *p;
  int b = *p;
  return a < b;
}
