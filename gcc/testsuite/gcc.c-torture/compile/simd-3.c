#include <float.h>

/* If double is not wider than float, we probably don't have DFmode,
   or at least it's not as wide as double.  */
#if DBL_MANT_DIG > FLT_MANT_DIG
typedef double floatvect2 __attribute__((vector_size (16)));

typedef union
{
    floatvect2 vector;
    double f[2];
}resfloatvect2;

void tempf(double *x, double *y)
{
        floatvect2 temp={x[0],x[1]};
        floatvect2 temp1={y[0],y[1]};
        resfloatvect2 temp2;
        temp2.vector=temp+temp1;
        x[0]=temp2.f[0];
        x[1]=temp2.f[1];
}
#endif
