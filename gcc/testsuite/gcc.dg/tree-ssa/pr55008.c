/* This used to fail to compile; see PR55008.  */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

typedef unsigned long long T;

void f(void)
{
    int a, *p;

    T b = 6309343725;

    if(*p ? (b = 1) : 0)
        if(b - (a = b /= 0) ? : (a + b))
            while(1);
}

