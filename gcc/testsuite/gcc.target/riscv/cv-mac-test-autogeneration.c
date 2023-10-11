/* { dg-do compile } */
/* { dg-require-effective-target cv_mac } */
/* { dg-options "-O2 -march=rv32im_xcvmac -mabi=ilp32" } */

int
foo0(int a, int b, int c)
{
    return a * b + c;
}

int
foo1(int a, int b, int c)
{
    return a - b * c;
}

/* { dg-final { scan-assembler-times "cv\\.mac" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.msu" 1 } } */
