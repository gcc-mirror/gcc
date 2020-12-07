/* { dg-do compile } */

int
ctzsi (unsigned int x)
{
  return __builtin_ctz (x);
}

/* Expect assembly like:

	ffs $0,$32,4(%ap),%r0

 */

/* { dg-final { scan-assembler "\tffs \\\$0,\\\$32," } } */
