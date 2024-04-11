/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include "merge-2.c"

int main(void)
{
    vnx32qi vnx32qi_x= {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
    vnx32qi vnx32qi_y= {101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132};
    vnx32qi vnx32qi_expect= {1,102,3,104,5,106,7,108,9,110,11,112,13,114,15,116,17,118,19,120,21,122,23,124,25,126,27,128,29,130,31,132};
    vnx32qi vnx32qi_real;
    merge0(vnx32qi_x,vnx32qi_y,&vnx32qi_real);
    for(int i=0; i<32; i++)
        if(vnx32qi_real[i]!=vnx32qi_expect[i]) {
            __builtin_abort();
        }

    vnx32uqi vnx32uqi_x= {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
    vnx32uqi vnx32uqi_y= {101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132};
    vnx32uqi vnx32uqi_expect= {1,102,3,104,5,106,7,108,9,110,11,112,13,114,15,116,17,118,19,120,21,122,23,124,25,126,27,128,29,130,31,132};
    vnx32uqi vnx32uqi_real;
    merge1(vnx32uqi_x,vnx32uqi_y,&vnx32uqi_real);
    for(int i=0; i<32; i++)
        if(vnx32uqi_real[i]!=vnx32uqi_expect[i]) {
            __builtin_abort();
        }


    vnx16hi vnx16hi_x= {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    vnx16hi vnx16hi_y= {101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116};
    vnx16hi vnx16hi_expect= {1,102,3,104,5,106,7,108,9,110,11,112,13,114,15,116};
    vnx16hi vnx16hi_real;
    merge2(vnx16hi_x,vnx16hi_y,&vnx16hi_real);
    for(int i=0; i<16; i++)
        if(vnx16hi_real[i]!=vnx16hi_expect[i]) {
            __builtin_abort();
        }

    vnx16uhi vnx16uhi_x= {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    vnx16uhi vnx16uhi_y= {101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116};
    vnx16uhi vnx16uhi_expect= {1,102,3,104,5,106,7,108,9,110,11,112,13,114,15,116};
    vnx16uhi vnx16uhi_real;
    merge3(vnx16uhi_x,vnx16uhi_y,&vnx16uhi_real);
    for(int i=0; i<16; i++)
        if(vnx16uhi_real[i]!=vnx16uhi_expect[i]) {
            __builtin_abort();
        }

    vnx8si vnx8si_x= {1,2,3,4,5,6,7,8};
    vnx8si vnx8si_y= {101,102,103,104,105,106,107,108};
    vnx8si vnx8si_expect= {1,102,3,104,5,106,7,108};
    vnx8si vnx8si_real;
    merge4(vnx8si_x,vnx8si_y,&vnx8si_real);
    for(int i=0; i<8; i++)
        if(vnx8si_real[i]!=vnx8si_expect[i]) {
            __builtin_abort();
        }

    vnx8usi vnx8usi_x= {1,2,3,4,5,6,7,8};
    vnx8usi vnx8usi_y= {101,102,103,104,105,106,107,108};
    vnx8usi vnx8usi_expect= {1,102,3,104,5,106,7,108};
    vnx8usi vnx8usi_real;
    merge5(vnx8usi_x,vnx8usi_y,&vnx8usi_real);
    for(int i=0; i<8; i++)
        if(vnx8usi_real[i]!=vnx8usi_expect[i]) {
            __builtin_abort();
        }

    vnx4di vnx4di_x= {1,2,3,4};
    vnx4di vnx4di_y= {101,102,103,104};
    vnx4di vnx4di_expect= {1,102,3,104};
    vnx4di vnx4di_real;
    merge6(vnx4di_x,vnx4di_y,&vnx4di_real);
    for(int i=0; i<4; i++)
        if(vnx4di_real[i]!=vnx4di_expect[i]) {
            __builtin_abort();
        }

    vnx4udi vnx4udi_x= {1,2,3,4};
    vnx4udi vnx4udi_y= {101,102,103,104};
    vnx4udi vnx4udi_expect= {1,102,3,104};
    vnx4udi vnx4udi_real;
    merge7(vnx4udi_x,vnx4udi_y,&vnx4udi_real);
    for(int i=0; i<4; i++)
        if(vnx4udi_real[i]!=vnx4udi_expect[i]) {
            __builtin_abort();
        }


    vnx16hf vnx16hf_x= {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0};
    vnx16hf vnx16hf_y= {1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1,9.1,10.1,11.1,12.1,13.1,14.1,15.1,16.1};
    vnx16hf vnx16hf_expect= {1.0,2.1,3.0,4.1,5.0,6.1,7.0,8.1,9.0,10.1,11.0,12.1,13.0,14.1,15.0,16.1};
    vnx16hf vnx16hf_real;
    merge8(vnx16hf_x,vnx16hf_y,&vnx16hf_real);
    for(int i=0; i<8; i++)
        if(vnx16hf_real[i]!=vnx16hf_expect[i]) {
            __builtin_abort();
        }

    vnx8sf vnx8sf_x= {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0};
    vnx8sf vnx8sf_y= {1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1};
    vnx8sf vnx8sf_expect= {1.0,2.1,3.0,4.1,5.0,6.1,7.0,8.1};
    vnx8sf vnx8sf_real;
    merge9(vnx8sf_x,vnx8sf_y,&vnx8sf_real);
    for(int i=0; i<4; i++)
        if(vnx8sf_real[i]!=vnx8sf_expect[i]) {
            __builtin_abort();
        }

    vnx4df vnx4df_x= {1.0,2.0,3.0,4.0};
    vnx4df vnx4df_y= {1.1,2.1,3.1,4.1};
    vnx4df vnx4df_expect= {1.0,2.1,3.0,4.1};
    vnx4df vnx4df_real;
    merge10(vnx4df_x,vnx4df_y,&vnx4df_real);
    for(int i=0; i<2; i++)
        if(vnx4df_real[i]!=vnx4df_expect[i]) {
            __builtin_abort();
        }

    return 0;
}
