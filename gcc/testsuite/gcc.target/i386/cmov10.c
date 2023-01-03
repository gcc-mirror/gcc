/* { dg-do compile } */
/* { dg-options "-O2" } */
int a, b, c, d;

int foo(int x)
{
    if (x == 0) {
        a = 3;
        b = 1;
        c = 4;
        d = 1;
    } else {
        a = 5;
        b = 9;
        c = 2;
        d = 7;
    }
    return x;
}
/* { dg-final { scan-assembler-times "cmpl" 1 } } */
/* { dg-final { scan-assembler-times "sbbl" 1 } } */
