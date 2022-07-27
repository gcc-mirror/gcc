/* { dg-do compile } */
/* { dg-options "-O2 -mbmi" } */

int f(int a, int b, int c)
{
    return (a ^ b) ^ (a | c);
}

/* { dg-final { scan-assembler "andn\[ \\t\]+" } } */
