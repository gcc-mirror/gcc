/* { dg-do run } */

/* Test of obscure case in token pasting in the preprocessor.
   I can't think of any way to make this problem provoke a syntax error.
   Based on a bug report by Manfred Hollstein.  */

#include <string.h>

#define SP1(x, y) SP2(x, y)
#define SP2(x, y) SP3(x##y)
#define SP3(x) #x
#define MZ -0

int
main(void)
{
    char *x = SP1(0,MZ);
    char *y = "0-0";  /* should be the expansion of SP1(0,MZ) */

    if(strcmp(x, y))
	return 1;
    else
	return 0;
}
