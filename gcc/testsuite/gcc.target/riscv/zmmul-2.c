/* { dg-do compile } */
/* { dg-options "-march=rv32iafdc_zmmul -mabi=ilp32" } */
int foo1(int a, int b)
{
    return a*b;
}

int foo2(int a, int b)
{
    return a/b;
}

int foo3(int a, int b)
{
    return a%b;
}

/* { dg-final { scan-assembler-times "mul\t" 1 } } */
/* { dg-final { scan-assembler-not "div\t" } } */
/* { dg-final { scan-assembler-not "rem\t" } } */