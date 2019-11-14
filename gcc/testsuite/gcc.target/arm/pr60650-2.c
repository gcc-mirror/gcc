/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-O2 -fno-omit-frame-pointer -march=armv7-a" } */

int a, h, j;
long long d, e, i;
int f;
int
fn1 (void *p1, int p2)
{
    switch (p2)
    case 8:
{
    register int b = *(long long *) p1, c asm ("r2");
    asm ("%0": "=r" (a), "=r" (c):"r" (b), "r" (0));
    *(long long *) p1 = c;
    }
}

int
fn2 ()
{
    int k;
    k = f;
    while (1)
    {
        fn1 (&i, sizeof i);
        e = d + k;
        switch (d)
        case 0:
        (
        {
            register int l asm ("r4");
            register int m asm ("r0");
            asm ("  .err  .endif\n\t": "=r" (h), "=r" (j):"r" (m),
            "r"
            (l));;
        });
    }
}
