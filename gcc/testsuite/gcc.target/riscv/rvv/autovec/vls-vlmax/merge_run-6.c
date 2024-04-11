/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include "merge-6.c"

int main(void)
{
    vnx4qi vnx4qi_x= {1,2,3,4};
    vnx4qi vnx4qi_y= {101,102,103,104};
    vnx4qi vnx4qi_expect= {1,102,3,104};
    vnx4qi vnx4qi_real;
    merge0(vnx4qi_x,vnx4qi_y,&vnx4qi_real);
    for(int i=0; i<4; i++)
        if(vnx4qi_real[i]!=vnx4qi_expect[i]) {
            __builtin_abort();
        }

    vnx4uqi vnx4uqi_x= {1,2,3,4};
    vnx4uqi vnx4uqi_y= {101,102,103,104};
    vnx4uqi vnx4uqi_expect= {1,102,3,104};
    vnx4uqi vnx4uqi_real;
    merge1(vnx4uqi_x,vnx4uqi_y,&vnx4uqi_real);
    for(int i=0; i<4; i++)
        if(vnx4uqi_real[i]!=vnx4uqi_expect[i]) {
            __builtin_abort();
        }

    vnx2hi vnx2hi_x= {1,2};
    vnx2hi vnx2hi_y= {101,102};
    vnx2hi vnx2hi_expect= {1,102};
    vnx2hi vnx2hi_real;
    merge2(vnx2hi_x,vnx2hi_y,&vnx2hi_real);
    for(int i=0; i<2; i++)
        if(vnx2hi_real[i]!=vnx2hi_expect[i]) {
            __builtin_abort();
        }

    vnx2uhi vnx2uhi_x= {1,2};
    vnx2uhi vnx2uhi_y= {101,102};
    vnx2uhi vnx2uhi_expect= {1,102};
    vnx2uhi vnx2uhi_real;
    merge3(vnx2uhi_x,vnx2uhi_y,&vnx2uhi_real);
    for(int i=0; i<2; i++)
        if(vnx2uhi_real[i]!=vnx2uhi_expect[i]) {
            __builtin_abort();
        }

    vnx2hf vnx2hf_x= {1.0,2.0};
    vnx2hf vnx2hf_y= {1.1,2.1};
    vnx2hf vnx2hf_expect= {1.0,2.1};
    vnx2hf vnx2hf_real;
    merge6(vnx2hf_x,vnx2hf_y,&vnx2hf_real);
    for(int i=0; i<2; i++)
        if(vnx2hf_real[i]!=vnx2hf_expect[i]) {
            __builtin_abort();
        }

    return 0;
}
