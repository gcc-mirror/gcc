/* { dg-do compile } */
/* { dg-options "-O2 -mxop -mno-avx512vl" } */
typedef int v4si __attribute__ ((vector_size (16)));

v4si foo(v4si c, v4si t, v4si f)
{
    return (c&t)|(~c&f);
}

/* { dg-final { scan-assembler "vpcmov" } } */
