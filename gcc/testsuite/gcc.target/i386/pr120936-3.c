/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -mnop-mcount -pg -mno-fentry -fno-pic -fno-shrink-wrap" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**	.byte	0x0f, 0x1f, 0x44, 0x00, 0x00
**...
*/

void
foo (void)
{
}
