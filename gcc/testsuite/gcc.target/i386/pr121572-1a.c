/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O0 -fpic -fplt -mtls-dialect=gnu" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**bug:
**.LFB[0-9]+:
**...
**	leaq	tv_cache@tlsld\(%rip\), %rdi
**	call	__tls_get_addr@PLT
**	movl	\$-1, %edi
**	mov[l|q]	%[e|r]ax, %[e|r]bx
**	call	val@PLT
**...
*/

extern __thread int tv_cache __attribute__ ((visibility ("hidden")));
extern void use_cache (int);
extern int val (int v);

__attribute__ ((optimize (2,"-fno-reorder-blocks-and-partition")))
void
bug (void)
{
  int compared = val (-1);

  if (compared == 0 || (compared > 0 && val (2) == 0))
    {
      __builtin_trap ();
    }

  if (compared < 0)
    {
      use_cache (tv_cache);
      return;
    }

  use_cache (tv_cache);
  __builtin_trap ();
}
