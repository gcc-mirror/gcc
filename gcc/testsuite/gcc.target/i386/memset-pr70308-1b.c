/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -fomit-frame-pointer -DUSE_SCANF" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	subq	\$136, %rsp
**	.cfi_def_cfa_offset 144
**	xorl	%eax, %eax
**	movl	\$.LC[0-9]+, %edi
**	movq	%rsp, %rsi
**	call	scanf
**	cmpl	\$1, %eax
**	je	.L[0-9]+
**	movl	\$42, %eax
**	addq	\$136, %rsp
**	.cfi_remember_state
**	.cfi_def_cfa_offset 8
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L[0-9]+:
**	.cfi_restore_state
**	pxor	%xmm0, %xmm0
**	movaps	%xmm0, \(%rsp\)
**	movaps	%xmm0, 16\(%rsp\)
**	movaps	%xmm0, 32\(%rsp\)
**	movaps	%xmm0, 48\(%rsp\)
**	movaps	%xmm0, 64\(%rsp\)
**	movaps	%xmm0, 80\(%rsp\)
**	movaps	%xmm0, 96\(%rsp\)
**	movaps	%xmm0, 112\(%rsp\)
**	xorl	%eax, %eax
**	addq	\$136, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**...
*/

extern int scanf (const char *, ...);
extern void *memset (void *, int, __SIZE_TYPE__);

int
foo (void)
{
  char buf[128];

#if USE_SCANF
  if (scanf("%s", buf) != 1)
    return 42;
#endif

  memset (buf,0, sizeof (buf));
  asm volatile("": : :"memory");
  return 0;
}

/* { dg-final { scan-assembler-not "rep stos" } } */
