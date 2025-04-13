/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -fno-pic -mtune=generic -msse2 -mno-apxf -mtune-ctrl=prologue_using_move,epilogue_using_move" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB[0-9]+:
**	.cfi_startproc
**	subq	\$8, %rsp
**	.cfi_def_cfa_offset 16
**	call	boring
**	addq	\$8, %rsp
**	.cfi_def_cfa_offset 8
**	jmp	\*continuation\(%rip\)
**	.cfi_endproc
**...
*/

extern void boring (void);

extern void (*continuation) (void *, void *, void *, void *)
  __attribute__ ((preserve_none));

__attribute__ ((preserve_none))
void
entry (void *a, void *b, void *c, void *d)
{
  boring ();
  continuation (a, b, c, d);
}
