/* { dg-do compile } */
/* { dg-options "-march=rv64iafdc_zmmul -mabi=lp64d" } */
int foo1(int a)
{
    return a * 999999;
}

/* { dg-final { scan-assembler-times "mulw\t" 1 } } */
