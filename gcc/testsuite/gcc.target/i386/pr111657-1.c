/* { dg-do assemble } */
/* { dg-options "-O2 -mno-sse -mtune=generic -save-temps" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**bar:
**...
**.L[0-9]+:
**	movl	%edx, %eax
**	addl	\$32, %edx
**	movq	%gs:m\(%rax\), %r9
**	movq	%gs:m\+8\(%rax\), %r8
**	movq	%gs:m\+16\(%rax\), %rsi
**	movq	%gs:m\+24\(%rax\), %rcx
**	movq	%r9, \(%rdi,%rax\)
**	movq	%r8, 8\(%rdi,%rax\)
**	movq	%rsi, 16\(%rdi,%rax\)
**	movq	%rcx, 24\(%rdi,%rax\)
**	cmpl	\$224, %edx
**	jb	.L[0-9]+
**...
*/

typedef unsigned long uword __attribute__ ((mode (word)));

struct a { uword arr[30]; };

__seg_gs struct a m;
void bar (struct a *dst) { *dst = m; }

/* { dg-final { scan-assembler-not "rep movs" } } */
