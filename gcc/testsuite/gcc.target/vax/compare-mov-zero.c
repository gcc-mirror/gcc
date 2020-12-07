/* { dg-do compile } */
/* { dg-options "" } */

int
compare_mov (int x)
{
  if (x > 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	movl 4(%ap),%r0
	jgtr .L2
	addl2 $2,%r0
.L2:

A reverse branch may be used at some optimization levels.  */

/* Make sure the comparison is made against 0 rather than 1.  */
/* { dg-final { scan-assembler-not "\tj(geq|lss) " } } */
/* { dg-final { scan-assembler "\tj(gtr|leq) " } } */
