/* { dg-do compile } */
/* { dg-options "" } */

int
compare_add (int x, int y)
{
  int z;

  z = x + y;
  if (z < 0)
    return z;
  else
    return z + 2;
}

/* Expect assembly like:

	addl3 4(%ap),8(%ap),%r0
	jlss .L1
	addl2 $2,%r0
.L1:

A reverse branch may be used at some optimization levels.  */

/* Make sure the comparison is made against 0 rather than -1.  */
/* { dg-final { scan-assembler-not "\tj(gtr|leq) " } } */
/* { dg-final { scan-assembler "\tj(geq|lss) " } } */
