/* PR rtl-optimization/25703 */
/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -mtune=pentiumpro" } */

extern void abort (void);

struct a
{
        int a;
        char b,c,d,e;
};

__attribute__ ((noinline))
__attribute__ ((regparm(1))) void t(struct a a)
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

