/* This ICEd prior to fixing PR57993.  */
/* { dg-do compile } */

int a, b, c, d;
char e;
unsigned g;

void f(void)
{
    int h;

    for(; d; d++)
        if(d)
lbl:
            g + a || (d = 0);

    b && (a = e);

    for(h = 0; h < 1; ++h)
    {
        h = c ? : (d = 0);
        g = a = (e | 0);
    }

    if(a)
        goto lbl;

    a = e = 0;
    goto lbl;
}
