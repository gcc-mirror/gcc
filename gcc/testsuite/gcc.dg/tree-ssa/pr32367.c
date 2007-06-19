/* { dg-do compile } */
/* { dg-options "-O2" } */

int BinomialCoefficientsInited = 0;
int BinomialCoefficients[17 * 35];
double Evaluate_TPat (void)
{
        unsigned short n, k;
        if (BinomialCoefficientsInited == 0)
        {
                int *ptr = BinomialCoefficients;
                for (n = 1; n <= 33; ++n)
                {
                        for (k = 1; k < n; ++k)
                                ++ptr;
                        *ptr = 1;
                }
        }
}

