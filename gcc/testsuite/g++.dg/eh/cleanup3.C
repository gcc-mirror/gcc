// C++/4794
//  This test used to hang in has_cleanups
//  Contributed by: <fritz@intrinsity.com>
/* { dg-do compile } */

void foo (int a,int b,int c,int d,int e,int f,int g,int h,int i,int j,int k,
	int l,int m,int n,int o,int p,int q,int r,int s,int t,int u,int v,int w,
	int x,int y,int z,int aa,int ab,int ac)
{ 
if (!((((ac != 0) + (d != 0) + (c != 0) + (f != 0) + (x != 0) + (y != 0) +
	(z != 0) + (aa != 0) + (ab != 0) + (g != 0) + (b != 0) + (a != 0) +
	(h != 0) + (e != 0) + (p != 0) + (q != 0) + (n != 0) + (r != 0) +
	(o != 0) + (v != 0) + (w != 0) + (t != 0) + (u != 0) + (s != 0) +
	(k != 0) + (l != 0) + (m != 0) + (i != 0) + (j != 0)) <= 1)))
	{
	return;
	}
}


