/* PR rtl-optimization/55833 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c;
int bar (void);

void foo()
{
    unsigned d, l, *p, k = 1;

    if(bar())
    {
label:
      	if((a = a <= 0))
        {
            if(c)
                d = b;

            if (b || d ? l : k ? : 0)
                a = d = 0;

            goto label;
       	}
    }

    while(*p++)
        goto label;
}
