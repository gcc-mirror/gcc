/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O3 -fPIC -march=x86-64-v4 -fno-asynchronous-unwind-tables -mtls-dialect=gnu" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**func:
**	pushq	%rbp
**	movq	%rsp, %rbp
**	pushq	%r12
**	pushq	%r10
**	leaq	16\(%rbp\), %r10
**	pushq	%rbx
**	movq	%r10, %r12
**	subq	\$8, %rsp
**	data16	leaq	FLA_ONE@tlsgd\(%rip\), %rdi
**	.value	0x6666
**	rex64
**	call	__tls_get_addr@PLT
**...
*/

typedef struct
{
   long n;
   long m_inner;
   long n_inner;
   int base;
 } FLA_Obj;
extern __thread FLA_Obj FLA_ONE, W12;
extern long FLA_Obj_length (FLA_Obj);
extern void FLA_Obj_width (FLA_Obj, ...);
void func (FLA_Obj A)
{
  while (FLA_Obj_length (A))
    FLA_Obj_width (FLA_ONE);
  FLA_Obj_width (FLA_ONE, W12);
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 2 } } */
