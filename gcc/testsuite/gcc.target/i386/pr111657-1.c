/* { dg-do assemble } */
/* { dg-options "-O2 -mno-sse -mtune=generic -save-temps" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**bar:
**...
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$32, %eax
**	movq	%gs:m\(%rdx\), %r9
**	movq	%gs:m\+8\(%rdx\), %r8
**	movq	%gs:m\+16\(%rdx\), %rsi
**	movq	%gs:m\+24\(%rdx\), %rcx
**	movq	%r9, \(%rdi,%rdx\)
**	movq	%r8, 8\(%rdi,%rdx\)
**	movq	%rsi, 16\(%rdi,%rdx\)
**	movq	%rcx, 24\(%rdi,%rdx\)
**	cmpl	\$224, %eax
**	jb	.L[0-9]+
**...
*/

typedef unsigned long uword __attribute__ ((mode (word)));

struct a { uword arr[30]; };

__seg_gs struct a m;
void bar (struct a *dst) { *dst = m; }

/* { dg-final { scan-assembler-not "rep movs" } } */
