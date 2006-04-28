/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O -fomit-frame-pointer -march=i586" } */

void foo(char* p, char c, int i)
{
    char a[2], *q=a+1;
    if (p && i)
        *p = q-a+bar(i);
    if (c)
        bar(i);
}
