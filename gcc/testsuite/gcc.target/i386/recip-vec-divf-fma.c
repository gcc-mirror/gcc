/* { dg-do compile } */
/* { dg-options "-Ofast -mfma -mavx2" } */
/* { dg-final { scan-assembler-times {(?n)vfn?m(add|sub)[1-3]*ps} 2 } } */

typedef float v4sf __attribute__((vector_size(16)));
/* (a - (rcp(b) * a * b)) * rcp(b) + rcp(b) * a  */

v4sf
foo (v4sf a, v4sf b)
{
    return a / b;
}
