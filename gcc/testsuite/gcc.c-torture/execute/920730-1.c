/* 920730-1.c */
#include <limits.h>
f1()
{
	int b=INT_MIN;
	return b>=INT_MIN;
}

f2()
{
	int b=INT_MIN+1;
	return b>= (unsigned)(INT_MAX+2);
}

f3()
{
	int b=INT_MAX;
	return b>=INT_MAX;
}

f4()
{
	int b=-1;
	return b>=UINT_MAX;
}

main ()
{
	if((f1()&f2()&f3()&f4())!=1)
		abort();
		exit(0);
}
