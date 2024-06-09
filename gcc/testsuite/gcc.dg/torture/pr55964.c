/* { dg-do compile } */
/* { dg-options "-ftree-loop-distribution -funswitch-loops -w" } */

int a, b;

void f(void)
{
lbl1:
    for(b = 0; b < 1; b++)
    {
        int u = 1;

        if((b %= 0) * (b ? 0 : a) - 1 && (u /= 0))
        {
            int *q = &u, **k = (int **) q;
            goto lbl1;
lbl2:
lbl3:
            a = **k;
            goto lbl2;
        }
    }
    goto lbl3;
}
