// GROUPS passed references

// execution test

#include <stdio.h>
#include <stdlib.h>

const int& min(const int& tX, const int& tY)
{
        return tX < tY ? tX : tY;
}

void foo(const int m, const int n)
{
	if (m == 1 && n == 100)
	  printf("PASS\n");
	else
	  abort ();
}

int main()
{
        foo(min(2, 1), min(100, 200));
        return 0;
}
