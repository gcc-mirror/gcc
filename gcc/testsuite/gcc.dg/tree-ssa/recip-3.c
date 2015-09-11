/* { dg-do compile } */
/* { dg-options "-O1 -fno-trapping-math -funsafe-math-optimizations -fdump-tree-recip" } */

double F[2] = { 0.0, 0.0 }, e;

/* In this case the optimization is interesting.  */
float h ()
{
	int i;
	double E, W, P, d;

	W = 1.1;
	d = 2.*e;
	E = 1. - d;

	for( i=0; i < 2; i++ )
		if( d > 0.01 )
		{
			P = ( W < E ) ? (W - E)/d : (E - W)/d;
			F[i] += P;
		}

	F[0] += E / d;
}

/* { dg-final { scan-tree-dump-times " / " 1 "recip" } } */
