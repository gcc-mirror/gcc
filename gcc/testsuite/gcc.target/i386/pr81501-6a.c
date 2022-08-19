/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64 -fpic -fplt -mtls-dialect=gnu" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**in_dso:
**.LFB[0-9]+:
**...
**	mov(l|q)	%(e|r)dx, %.*
**...
**	movl	%edi, %.*
**...
**	mov(l|q)	%(e|r)si, %.*
**...
**	call	__tls_get_addr@PLT
**...
*/

__thread int foo;
__thread int bar;

extern void fun1 (int *, int *);
extern void fun2 (int);
extern void fun3 (const char *);

int
in_dso (int n, int *caller_foop, int *caller_barp)
{
  int *foop;
  int *barp;
  int result = 0;

  fun3 ("foo");			/* Make sure PLT is used before macros.  */
  asm ("" ::: "memory");

  foop = &foo;
  barp = &bar;

  if (caller_foop != (void *) 0 && foop != caller_foop)
    {
      fun1 (caller_foop, foop);
      result = 1;
      if (caller_barp != (void *) 0 && barp != caller_barp)
	{
	  fun1 (caller_barp, barp);
	  result = 2;
	}
      else if (*barp != n)
	{
	  fun2 (n);
	  result = 3;
	}
    }
  else if (*foop != n)
    {
      fun2 (n);
      result = 4;
    }

  *barp = 16;
  *foop = 16;

  return result;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 2 { target { ! ia32 } } } } */
