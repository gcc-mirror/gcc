/* Check that a conditional return is used.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fpermissive -w" } */

/* { dg-final { scan-assembler {\mbeqlr\M} } } */


int f(int x)
{
	if (x)
		return x + 31;

	return;
}
