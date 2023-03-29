/* { dg-do compile } */

/* PR middle-end/106982 */

long long n = 100;
int multiplicitive_n = 128;

void test1(double *rand, double *a, double *b, double *c)
{
#pragma acc data copyin(a[0:10*multiplicitive_n], b[0:10*multiplicitive_n]) copyout(c[0:10])
    {
#pragma acc parallel loop
        for (int i = 0; i < 10; ++i)
        {
        double temp = 1.0;
#pragma acc loop vector reduction(*:temp)
        for (int j = 0; j < multiplicitive_n; ++j)
          temp *= a[(i * multiplicitive_n) + j] + b[(i * multiplicitive_n) + j];
        c[i] = temp;
        }
    }
}
