#include <stdio.h>

int acker(int, int);

int
main(void)
{
    int n = acker(3,6);
    if (n != 509)
	printf("acker(3,6) = %d != 509\n", n);
    return(0);
}

int
acker(int x,int y)
{
    if (x==0)
	return(y+1);
    else if (y==0)
	return(acker(x-1,1));
    else
	return(acker(x-1, acker(x, y-1)));
}
