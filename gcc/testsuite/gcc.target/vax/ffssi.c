/* { dg-do compile } */

int
ffssi (int x)
{
  return __builtin_ffs (x);
}

/* Expect assembly like:

	ffs $0,$32,%r1,%r0
	jneq .L2
	mnegl $1,%r0
.L2:
	incl %r0

 */

/* { dg-final { scan-assembler "\tffs \\\$0,\\\$32," } } */
