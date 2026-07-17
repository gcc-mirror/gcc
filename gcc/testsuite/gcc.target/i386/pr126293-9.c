/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=x86-64 -m128bit-atomic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movdqa	load\(%rip\), %xmm0
**	ret
**	.cfi_endproc
**...
*/

typedef float vector __attribute__((vector_size (16)));
extern _Atomic vector load;

vector
func (void)
{
  return load;
}
