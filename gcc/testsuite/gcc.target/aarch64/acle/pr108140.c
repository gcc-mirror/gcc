/* PR target/108140  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include<arm_acle.h>

int
main(int argc, char *argv[])
{
	unsigned long long input = argc-1;
	unsigned long long v = __clz(__rbit(input));
	__builtin_printf("%d %d\n", argc, v >= 64 ? 123 : 456);
	return 0;
}

