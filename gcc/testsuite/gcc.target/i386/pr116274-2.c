/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-slp2-optimized" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target  { *-*-linux* && lp64 } } {^\t?\.}  } } */

/*
**test:
**.LFB[0-9]+:
**	.cfi_startproc
**	leaq	\(%rdi,%rsi\), %rax
**	ret
**	.cfi_endproc
**...
*/

struct a { long x,y; };
long test(struct a a) { return a.x+a.y; }

/* { dg-final { scan-tree-dump-not "basic block part vectorized" "slp2" } } */
/* { dg-final { scan-assembler-times "addl|leaq" 1 } } */
/* { dg-final { scan-assembler-not "padd" } } */
