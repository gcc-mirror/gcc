/* PR tree-optimization/58011 */
/* { dg-do compile } */
/* { dg-options "-O1" } */
int a, b;

void f(unsigned p)
{
    unsigned *pp = &p;

    if(!a)
        p = 0;

    for(b = 0; b < 1; b++)
        if(3 * p + 5 * *pp)
            a = 0;
}

