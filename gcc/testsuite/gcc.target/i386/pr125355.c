/* { dg-do compile } */
/* { dg-options "-Os -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	jmp	_?memmove
**	.cfi_endproc
**...
*/

#include <stddef.h>
#include <string.h>

void
foo (char *d, char *s, size_t n)
{
  memmove(d, s, n);
}
