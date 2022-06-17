/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x)
{
    return (x & 1234) == 1234;
}

int foos(short x)
{
    return (x & 1234) == 1234;
}

int fooc(char x)
{
    return (x & 123) == 123;
}

int fool(long long x)
{
    return (x & 1234) == 1234;
}

/* { dg-final { scan-assembler-not "cmp" } } */
