/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text == 12 } } */

unsigned long long test(void)
{
	return 0xffffffff00000000UL;
}
