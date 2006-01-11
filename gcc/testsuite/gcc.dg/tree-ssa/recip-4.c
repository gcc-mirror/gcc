/* { dg-do compile } */
/* { dg-options "-O1 -fno-trapping-math -funsafe-math-optimizations -fdump-tree-recip" } */

/* based on the test case in pr23109 */

double F[2] = { 0., 0. }, e = 0.;

/* Nope, we cannot prove the optimization is worthwhile in this case.  */
void f ()
{
	int i;
	double E, W, P, d;

	W = 1.;
	d = 2.*e;
	E = 1. - d;

	if( d > 0.01 )
	{
		P = ( W < E ) ? (W - E)/d : (E - W)/d;
		F[i] += P;
	}
}

/* We also cannot prove the optimization is worthwhile in this case.  */
float g ()
{
	int i;
	double E, W, P, d;

	W = 1.;
	d = 2.*e;
	E = 1. - d;

	if( d > 0.01 )
	{
		P = ( W < E ) ? (W - E)/d : (E - W)/d;
		F[i] += P;
	}

	return 1.0 / d;
}

/* { dg-final { scan-tree-dump-not "reciptmp" "recip" } } */
/* { dg-final { cleanup-tree-dump "recip" } } */
