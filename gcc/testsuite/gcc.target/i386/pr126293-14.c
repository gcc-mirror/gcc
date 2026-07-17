/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=x86-64 -m128bit-atomic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movq	%rdi, %xmm0
**	movq	%rsi, %xmm1
**	punpcklqdq	%xmm1, %xmm0
**	movaps	%xmm0, store\(%rip\)
**	lock orq	\$0, \(%rsp\)
**	ret
**	.cfi_endproc
**...
*/

typedef struct
{
  int a[4];
} s;

extern _Atomic s store;

void
func (s i)
{
  store = i;
}
