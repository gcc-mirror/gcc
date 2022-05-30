/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

int foo(int x, int y)
{
    return (x & y) == y;
}

int foos(short x, short y)
{
    return (x & y) == y;
}

int fooc(char x, char y)
{
    return (x & y) == y;
}

int fool(long long x, long long y)
{
    return (x & y) == y;
}

/* { dg-final { scan-assembler-not "cmp" } } */
