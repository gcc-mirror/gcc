/* { dg-options "-Ofast -mno-soft-cmpsf" } */

#if 0
#include <math.h>
#else
extern float fminf (float, float);
#endif
void test(int a, float *b, int n)
{
    if (n < 10) {
        *b = a*fminf(0.0,1.0*a);
    }
}
