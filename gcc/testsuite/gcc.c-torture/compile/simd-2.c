typedef float floatvect2 __attribute__((mode(V2SF)));

typedef union
{
    floatvect2 vector;
    float f[2];
}resfloatvect2;

void tempf(float *x, float *y)
{
        floatvect2 temp={x[0],x[1]};
        floatvect2 temp1={y[0],y[1]};
        resfloatvect2 temp2;
        temp2.vector=temp+temp1;
        x[0]=temp2.f[0];
        x[1]=temp2.f[1];
}
