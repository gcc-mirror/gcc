/* Test against a problem in push_reload.  */

/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2" } */

unsigned long foo (unsigned long long x, unsigned long y)
{
    unsigned long a;

    x += y;

    asm ("" : "=a" (a) : "A" (x), "rm" (y));

    return a;
}
