/* Verify that LRA does not abort prematurely in a release build of the
   compiler.  */
/* { dg-do compile } */
/* { dg-options "-fno-checking -w -funroll-loops" } */

int a, b, c;

void f(p1)
{
    for(;;)
    {
        if(p1 ? : (c /= 0))
        {
            int d;

            for(; d; d++)
            {
                for(b = 0; b < 4; b++)
                    p1 /= p1;
lbl:
                while(a);
            }
        }

        if((c &= 1))
            goto lbl;
    }
}
