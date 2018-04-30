/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, *c;
void d(void)
{
    int **e;
    for(;;)
        for(int f = 1; f <= 6; f++)
        {
            b = 0;
            if(a)
g:
                while(a++);
            if (**e);
            else
            {
                *c = a;
                goto g;
            }
        }
}
