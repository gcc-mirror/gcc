/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -g0 -fcf-protection=branch" } */
/* Keep labels and directives ('.p2align', '.cfi_startproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
** (
**	endbr64
**	.p2align 5
** |
**	endbr32
** )
**...
*/
char *
foo (char *dest, const char *src)
{
  while ((*dest++ = *src++) != '\0')
    /* nothing */;
  return --dest;
}
