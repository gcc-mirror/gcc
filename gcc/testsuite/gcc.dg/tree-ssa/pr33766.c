/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear" } */

float
fxt1_quantize_ALPHA1()
{
        int j1;
        int i;
        float *tv;
        for (j1 = 1; j1; j1++) {
                float e;
                for (i = 1; i; i++)
                        e = tv[i];
                if (e)
                        i = j1;
        }
        return tv[i];
}

