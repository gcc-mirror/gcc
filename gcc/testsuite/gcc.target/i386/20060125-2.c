/* PR rtl-optimization/25703 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mtune=pentiumpro" } */

extern void abort (void);

struct a
{
        int a;
        char b,c,d,e;
};

__attribute__ ((noinline))
__attribute__ ((regparm(1))) t(struct a a)
{
        if (a.a!=1 || a.b!=1 || a.c!=1)
                        abort();
}

int main()
{
        struct a a;
        a.c=1;
        a.a=1;
        a.b=1;
        t(a);
        return 0;
}

