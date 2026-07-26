/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	subq	\$88, %rsp
**	.cfi_def_cfa_offset 96
**	leaq	96\(%rsp\), %rax
**	movq	%r13, 40\(%rsp\)
**	movl	40\(%rsp\), %r13d
**	movq	%r14, 48\(%rsp\)
**	movl	48\(%rsp\), %r14d
**	movq	%r15, 56\(%rsp\)
**	movl	56\(%rsp\), %r15d
**	movq	%rdi, 64\(%rsp\)
**	movl	64\(%rsp\), %edi
**	movq	%rsi, 72\(%rsp\)
**	movl	72\(%rsp\), %esi
**	movq	%rax, 16\(%rsp\)
**	leaq	32\(%rsp\), %rax
**	movq	%rax, 24\(%rsp\)
**	xorl	%eax, %eax
**	movl	\$8, 8\(%rsp\)
**	call	continuation
**	addq	\$88, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

#include <stdarg.h>

#ifndef CALLEE_ATTRIBUTE
#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none, sysv_abi))
#endif

#ifndef CALLER_ATTRIBUTE
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none, sysv_abi))
#endif

extern void continuation (int, ...) CALLEE_ATTRIBUTE;

CALLER_ATTRIBUTE
void
entry (int i, ...)
{
  va_list argp;
  va_start (argp, i);
  int arg1 = va_arg (argp, int);
  int arg2 = va_arg (argp, int);
  int arg3 = va_arg (argp, int);
  int arg4 = va_arg (argp, int);
  int arg5 = va_arg (argp, int);
  continuation (i, arg1, arg2, arg3, arg4, arg5);
}
