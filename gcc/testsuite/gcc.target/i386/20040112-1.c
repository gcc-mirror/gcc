/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**ftn:
**...
**.L[0-9]+:
**	movb	\$-24, \(%(e|r)(di|ax)\)
**	cmpb	\$0, \(%(e|r)(di|ax)\)
**	jns	.L[0-9]+
**	ret
**	.cfi_endproc
**...
*/

void
ftn (char *sp)
{
  char status;

  while (1)
    {
      *sp = 0xE8;
      status = *(volatile char *) sp;
      if (status & 0x80)
	break;
    }
}
