/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64 -fpic -fplt -mtls-dialect=gnu" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**in_dso:
**.LFB[0-9]+:
**...
**	movl	%edi, %.*
**...
**	mov(l|q)	%(e|r)si, %.*
**...
**	call	__tls_get_addr@PLT
**...
*/

__thread int foo;

extern void bar1 (int *, int *);
extern void bar2 (int);
extern void bar3 (const char *);

int
in_dso (int n, int *caller_foop)
{
  int *foop;
  int result = 0;

  bar3 ("foo");			/* Make sure PLT is used before macros.  */
  asm ("" ::: "memory");

  foop = &foo;

  if (caller_foop != (void *) 0 && foop != caller_foop)
    {
      bar1 (caller_foop, foop);
      result = 1;
    }
  else if (*foop != n)
    {
      bar2 (n);
      result = 1;
    }

  *foop = 16;

  return result;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */
