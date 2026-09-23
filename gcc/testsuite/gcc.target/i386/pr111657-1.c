/* { dg-do assemble } */
/* { dg-options "-O2 -mno-sse -mtune=generic -save-temps" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "*#" "" { target { lp64 && { ! *-*-darwin* } } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { lp64 && *-*-darwin* } } {^\t?\.} } } */

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
*#

* Darwin caches the address of m in rax.
*Dbar:
*D...
*DL[0-9]+:
*D	movl	%edx, %ecx
*D	addl	\$32, %edx
*D	movq	%gs:\(%rax,%rcx\), %r10
*D	movq	%gs:8\(%rax,%rcx\), %r9
*D	movq	%gs:16\(%rax,%rcx\), %r8
*D	movq	%gs:24\(%rax,%rcx\), %rsi
*D	movq	%r10, \(%rdi,%rcx\)
*D	movq	%r9, 8\(%rdi,%rcx\)
*D	movq	%r8, 16\(%rdi,%rcx\)
*D	movq	%rsi, 24\(%rdi,%rcx\)
*D	cmpl	\$224, %edx
*D	jb	L[0-9]+
*D...
*E
*/

typedef unsigned long uword __attribute__ ((mode (word)));

struct a { uword arr[30]; };

__seg_gs struct a m;
void bar (struct a *dst) { *dst = m; }

/* { dg-final { scan-assembler-not "rep movs" } } */
