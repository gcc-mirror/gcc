/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include "merge-7.c"

int main(void)
{
    vnx2qi vnx2qi_x= {1,2};
    vnx2qi vnx2qi_y= {101,102};
    vnx2qi vnx2qi_expect= {1,102};
    vnx2qi vnx2qi_real;
    merge0(vnx2qi_x,vnx2qi_y,&vnx2qi_real);
    for(int i=0; i<2; i++)
        if(vnx2qi_real[i]!=vnx2qi_expect[i]) {
            __builtin_abort();
        }

    vnx2uqi vnx2uqi_x= {1,2};
    vnx2uqi vnx2uqi_y= {101,102};
    vnx2uqi vnx2uqi_expect= {1,102};
    vnx2uqi vnx2uqi_real;
    merge1(vnx2uqi_x,vnx2uqi_y,&vnx2uqi_real);
    for(int i=0; i<2; i++)
        if(vnx2uqi_real[i]!=vnx2uqi_expect[i]) {
            __builtin_abort();
        }

    return 0;
}
