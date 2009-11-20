/* PR debug/41926 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -ffast-math -funroll-loops -ftree-vectorize -msse2" { target { i?86-*-* x86_64-*-* } } } */

void
foo (double (*__restrict p)[4], double (*__restrict q)[4],
     double *__restrict prim, double scale, double pp, double pq)
{
  int md, mc, mb, ma, p_index = 0;

  for (md = 0; md < 1; md++)
    for (mc = 0; mc < 1; mc++)
      for (mb = 0; mb < 1; mb++)
	for (ma = 0; ma < 4; ma++)
	  {
	    double tmp = scale * prim[p_index++];
	    p[md][ma] = p[md][ma] - tmp * pp;
	    q[mc][ma] = q[mc][ma] - tmp * pq;
	  }
}
