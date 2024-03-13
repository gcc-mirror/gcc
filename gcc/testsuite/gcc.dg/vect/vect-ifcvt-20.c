/* { dg-do compile } */
/* { dg-additional-options "-fgimple -fopenmp-simd -Ofast -fdump-tree-ifcvt-raw" } */

void foo (int * restrict p, int * restrict q, int * restrict r, int * restrict s, int * restrict t, int * restrict u)
{
#pragma omp simd
  for (int i = 0; i < 1024; i++)
    {
      int vp = p[i];
      int vq = q[i];
      int vr = r[i];
      int vs = s[i];
      int vt = t[i];
      int vu = u[i];
      int vw;
      if (vp != 0)
	{
	  if (vp > 100)
	    {
	      if (vq < 200)
		vw = 1;
	      else if (vr)
		vw = 2;
	      else
		vw = 3;
	    }
	  else if (vs > 100)
	    {
	      if (vq < 180)
		vw = 4;
	      else if (vr > 20)
		vw = 5;
	      else
		vw = 6;
	    }
	  else
	    {
	      if (vq < -100)
		vw = 7;
	      else if (vr < -20)
		vw = 8;
	      else
		vw = 9;
	    }
	}
      else if (vt > 10)
	{
	  if (vu > 100)
	    vw = 10;
	  else if (vu < -100)
	    vw = 11;
	  else
	    vw = 12;
	}
      else
	vw = 13;
      u[i] = vw;
    }
}

/* { dg-final { scan-tree-dump-times {<cond_expr,} 12 ifcvt { target vect_float } } } */
/* { dg-final { scan-tree-dump-times {<bit_and_expr,} 20 ifcvt { target vect_float } } } */
