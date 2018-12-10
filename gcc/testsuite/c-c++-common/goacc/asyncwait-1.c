void
f (int N, float *a, float *b)
{
    int ii;

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1 2) /* { dg-error "expected '\\)' before numeric constant" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1,) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (,1) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1,2,) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1,2 3) /* { dg-error "expected '\\)' before numeric constant" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1,2,,) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1 /* { dg-error "expected '\\)' before end of line" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (*) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (a)
	/* { dg-error "expected integer expression before" "" { target c } .-1 } */
	/* { dg-error "'async' expression must be integral" "" { target c++ } .-2 } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async (1.0)
	/* { dg-error "expected integer expression before" "" { target c } .-1 } */
	/* { dg-error "'async' expression must be integral" "" { target c++ } .-2 } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async () /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) async
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1 2) /* { dg-error "expected '\\)' before numeric constant" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1,) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (,1) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1,2,) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1,2 3) /* { dg-error "expected '\\)' before numeric constant" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1,2,,) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1 /* { dg-error "expected '\\\)' before end of line" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1,*) /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1,a) /*{ dg-error "must be integral" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (a) /* { dg-error "must be integral" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait (1.0) /* { dg-error "must be integral" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait () /* { dg-error "expected (integer |)expression (list |)before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait ( /* { dg-error "expected (primary-|)expression before" } */
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc parallel copyin (a[0:N]) copy (b[0:N]) wait
    {
        for (ii = 0; ii < N; ii++)
            b[ii] = a[ii];
    }

#pragma acc wait (1 2) /* { dg-error "expected '\\)' before numeric constant" } */

#pragma acc wait (1,) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait (,1) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait (1,2,) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait (1,2 3) /* { dg-error "expected '\\)' before numeric constant" } */

#pragma acc wait (1,2,,) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait (1 /* { dg-error "expected '\\\)' before end of line" } */

#pragma acc wait (1,*) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait (1,a) /* { dg-error "expression must be integral" } */

#pragma acc wait (a) /* { dg-error "expression must be integral" } */

#pragma acc wait (1.0) /* { dg-error "expression must be integral" } */

#pragma acc wait 1 /* { dg-error "expected '#pragma acc' clause before numeric constant" } */

#pragma acc wait N /* { dg-error "expected '#pragma acc' clause before 'N'" } */

#pragma acc wait async (1 2) /* { dg-error "expected '\\)' before numeric constant" } */

#pragma acc wait async (1 2) /* { dg-error "expected '\\)' before numeric constant" } */

#pragma acc wait async (1,) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait async (,1) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait async (1,2,) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait async (1,2 3) /* { dg-error "expected '\\)' before numeric constant" } */

#pragma acc wait async (1,2,,) /* { dg-error "expected (primary-|)expression before" } */

#pragma acc wait async (1 /* { dg-error "expected '\\)' before end of line" } */

#pragma acc wait async (*) /* { dg-error "expected (primary-|)expression before " } */

#pragma acc wait async (a)
    /* { dg-error "expected integer expression before" "" { target c } .-1 } */
    /* { dg-error "expression must be integral" "" { target c++ } .-2 } */

#pragma acc wait async (1.0)
   /* { dg-error "expected integer expression before" "" { target c } .-1 } */
   /* { dg-error "expression must be integral" "" { target c++ } .-2 } */
}
