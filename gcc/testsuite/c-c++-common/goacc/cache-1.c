/* OpenACC cache directive: valid usage.  */
/* For execution testing, this file is "#include"d from
   libgomp/testsuite/libgomp.oacc-c-c++-common/cache-1.c.  */

int
main (int argc, char **argv)
{
#define N   2
    int a[N], b[N];
    int i;

    for (i = 0; i < N; i++)
    {
        a[i] = 3;
        b[i] = 0;
    }

#pragma acc parallel copyin (a[0:N]) copyout (b[0:N])
{
    int ii;

    for (ii = 0; ii < N; ii++)
    {
        const int idx = ii;
        int n = 1;
        const int len = n;

	/* Have at it, GCC!  */
#pragma acc cache (a[0:N])
#pragma acc cache (a[0:N], a[0:N])
#pragma acc cache (a[0:N], b[0:N])
#pragma acc cache (a[0])
#pragma acc cache (a[0], a[1], b[0:N])
#pragma acc cache (a[i - 5])
#pragma acc cache (a[i + 5:len])
#pragma acc cache (a[i + 5:len - 1])
#pragma acc cache (b[i])
#pragma acc cache (b[i:len])
#pragma acc cache (a[ii])
#pragma acc cache (a[ii:len])
#pragma acc cache (b[ii - 1])
#pragma acc cache (b[ii - 1:len])
#pragma acc cache (b[i - ii + 1])
#pragma acc cache (b[i + ii - 1:len])
#pragma acc cache (b[i * ii - 1:len + 1])
#pragma acc cache (a[idx + 2])
#pragma acc cache (a[idx:len + 2])
#pragma acc cache (a[idx])
#pragma acc cache (a[idx:len])
#pragma acc cache (a[idx + 2:len])
#pragma acc cache (a[idx + 2 + i:len])
#pragma acc cache (a[idx + 2 + i + ii:len])

        b[ii] = a[ii];
    }
}


    for (i = 0; i < N; i++)
    {
        if (a[i] != b[i])
            __builtin_abort ();
    }

    return 0;
}
