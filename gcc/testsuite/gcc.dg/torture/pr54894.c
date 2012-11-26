/* { dg-do compile } */

typedef unsigned long long uint64_t;

#define n 4096
double A[n][n] __attribute__((aligned(16)));
double B[n][n] __attribute__((aligned(16)));
double C[n][n] __attribute__((aligned(16)));

#define tilesize 128

typedef double adouble __attribute__((__aligned__(16)));

void foo ()
{
  int ih, jh, kh, il, kl, jl;
  for (ih = 0; ih < n; ih += tilesize) 
    for (jh = 0; jh < n; jh += tilesize)                 
      for (kh = 0; kh < n; kh += tilesize)                      
	for (il = 0; il < tilesize; ++il)
	  {
	    adouble *Ap = (adouble *)&A[ih+il][kh];
	    for (kl = 0; kl < tilesize; ++kl)
	      for (jl = 0; jl < tilesize; ++jl)
		C[ih+il][jh+jl] += Ap[kl] * B[kh+kl][jh+jl];
	  }
}
