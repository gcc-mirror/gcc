/* OpenACC 'cache' directive: invalid usage.  */

/* See also corresponding C++ variant: '../../g++.dg/goacc/cache-2.C'.  */

static void
test ()
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

#pragma acc cache /* { dg-error "expected '\\\(' before end of line" } */
#pragma acc cache a[0:N] /* { dg-error "expected '\\\(' before 'a'" } */
	/* { dg-bogus "expected end of line before 'a'" "" { xfail c++ } .-1 } */
#pragma acc cache (a) /* { dg-error "expected '\\\['" } */
#pragma acc cache ( /* { dg-error "expected (identifier|unqualified-id) before end of line" } */
#pragma acc cache () /* { dg-error "expected (identifier|unqualified-id) before '\\\)' token" } */
#pragma acc cache (,) /* { dg-error "expected (identifier|unqualified-id) before '(,|\\\))' token" } */
#pragma acc cache (a[0:N] /* { dg-error "expected '\\\)' before end of line" } */
#pragma acc cache (a[0:N],) /* { dg-error "expected (identifier|unqualified-id) before '(,|\\\))' token" "" { xfail c } } */
#pragma acc cache (a[0:N]) copyin (a[0:N]) /* { dg-error "expected end of line before 'copyin'" } */
#pragma acc cache () /* { dg-error "expected (identifier|unqualified-id) before '\\\)' token" } */
#pragma acc cache (a[0:N] b[0:N]) /* { dg-error "expected '\\\)' before 'b'" } */
#pragma acc cache (a[0:N] b[0:N}) /* { dg-error "expected '\\\)' before 'b'" } */
	/* { dg-bogus "expected end of line before '\\\}' token" "" { xfail c++ } .-1 } */
#pragma acc cache (a[0:N] /* { dg-error "expected '\\\)' before end of line" } */
#pragma acc cache (a[0:N]) ( /* { dg-error "expected end of line before '\\(' token" } */
#pragma acc cache (a[0:N]) ii /* { dg-error "expected end of line before 'ii'" } */
#pragma acc cache (a[0:N] ii) /* { dg-error "expected '\\)' before 'ii'" } */

        b[ii] = a[ii];
    }
}


    for (i = 0; i < N; i++)
    {
        if (a[i] != b[i])
            __builtin_abort ();
    }
}
