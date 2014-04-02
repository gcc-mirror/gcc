/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -march=armv7-a" } */

int a, h, j;
long long d, e, i;
int f;
fn1 (void *p1, int p2)
{
    switch (p2)
    case 8:
{
    register b = *(long long *) p1, c asm ("r2");
    asm ("%0": "=r" (a), "=r" (c):"r" (b), "r" (0));
    *(long long *) p1 = c;
    }
}

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
            register l asm ("r4");
            register m asm ("r0");
            asm ("  .err  .endif\n\t": "=r" (h), "=r" (j):"r" (m),
            "r"
            (l));;
        });
    }
}
