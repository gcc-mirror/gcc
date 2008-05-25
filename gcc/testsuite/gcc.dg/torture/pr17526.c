/* { dg-do run } */
/* { dg-options "-fno-pcc-struct-return" { target i?86-*-* } } */

void abort(void);

typedef struct { int i; } A;

A __attribute__((noinline))
foo(void)
{
    A a = { -1 };
    return a;
}

void __attribute__((noinline))
bar(A *p)
{
    *p = foo();
}

int main(void)
{
    A a;
    bar(&a);
    if (a.i != -1) abort();
    return 0;
}
