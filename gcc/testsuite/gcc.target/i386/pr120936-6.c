/* { dg-do compile { target { *-*-linux* && fpic } } } */
/* { dg-options "-O2 -mrecord-mcount -mnop-mcount -pg -mno-fentry -fno-pic -fno-shrink-wrap" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^(1|\t?\.)} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**1:	.byte	0x0f, 0x1f, 0x44, 0x00, 0x00
**...
*/

void
foo (void)
{
}
