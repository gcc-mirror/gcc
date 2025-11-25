/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.}  } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	vpbroadcastw	cost\(%rip\), %xmm0
**	vmovq	%xmm0, cost1\(%rip\)
**	vmovdqu	%xmm0, cost2\(%rip\)
**	ret
**...
*/

extern struct {
  short cost[4];
} cost1;
extern struct {
  short cost[8];
} cost2;
extern int cost;

void
foo (void)
{
  cost1.cost[0] = cost;
  cost1.cost[1] = cost;
  cost1.cost[2] = cost;
  cost1.cost[3] = cost;
  cost2.cost[0] = cost;
  cost2.cost[1] = cost;
  cost2.cost[2] = cost;
  cost2.cost[3] = cost;
  cost2.cost[4] = cost;
  cost2.cost[5] = cost;
  cost2.cost[6] = cost;
  cost2.cost[7] = cost;
}
