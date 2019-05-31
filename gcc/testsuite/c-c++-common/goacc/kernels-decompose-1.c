/* Test OpenACC 'kernels' construct decomposition.  */

/* { dg-additional-options "-fopt-info-optimized-omp" } */
/* { dg-additional-options "-O2" } for "parloops".  */

/* See also "../../gfortran.dg/goacc/kernels-decompose-1.f95".  */

#pragma acc routine gang
extern int
f_g (int);

#pragma acc routine worker
extern int
f_w (int);

#pragma acc routine vector
extern int
f_v (int);

#pragma acc routine seq
extern int
f_s (int);

int
main ()
{
  int x, y, z;
#define N 10
  int a[N], b[N], c[N];

#pragma acc kernels
  {
    x = 0; /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" } */
    y = x < 10;
    z = x++;
    ;
  }

#pragma acc kernels
  for (int i = 0; i < N; i++) /* { dg-message "optimized: beginning .parloops. region in OpenACC .kernels. construct" } */
    a[i] = 0;

#pragma acc kernels loop
  /* { dg-message "optimized: forwarded loop nest in OpenACC .kernels. construct to .parloops. for analysis" "" { target *-*-* } .-1 } */
  for (int i = 0; i < N; i++)
    b[i] = a[N - i - 1];

#pragma acc kernels
  {
#pragma acc loop
    /* { dg-message "optimized: forwarded loop nest in OpenACC .kernels. construct to .parloops. for analysis" "" { target *-*-* } .-1 } */
    for (int i = 0; i < N; i++)
      b[i] = a[N - i - 1];

#pragma acc loop
    /* { dg-message "optimized: forwarded loop nest in OpenACC .kernels. construct to .parloops. for analysis" "" { target *-*-* } .-1 } */
    for (int i = 0; i < N; i++)
      c[i] = a[i] * b[i];

    a[z] = 0; /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" } */

#pragma acc loop
    /* { dg-message "optimized: forwarded loop nest in OpenACC .kernels. construct to .parloops. for analysis" "" { target *-*-* } .-1 } */
    for (int i = 0; i < N; i++)
      c[i] += a[i];

#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "optimized: parallelized loop nest in OpenACC .kernels. construct" "" { target *-*-* } .-1 } */
    for (int i = 0 + 1; i < N; i++)
      c[i] += c[i - 1];
  }

#pragma acc kernels
  {
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
    /* { dg-message "optimized: parallelized loop nest in OpenACC .kernels. construct" "" { target *-*-* } .-1 } */
    for (int i = 0; i < N; ++i)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
      for (int j = 0; j < N; ++j)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	 /* { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } */
	for (int k = 0; k < N; ++k)
	  a[(i + j + k) % N]
	    = b[j]
	    + f_v (c[k]); /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */

    //TODO Should the following turn into "gang-single" instead of "parloops"?
    //TODO The problem is that the first STMT is "if (y <= 4) goto <D.2547>; else goto <D.2548>;", thus "parloops".
    if (y < 5) /* { dg-message "optimized: beginning .parloops. region in OpenACC .kernels. construct" } */
#pragma acc loop independent /* { dg-message "optimized: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" } */
      for (int j = 0; j < N; ++j)
	b[j] = f_w (c[j]);
  }

#pragma acc kernels /* { dg-warning "region contains gang partitioned code but is not gang partitioned" } */
  {
    /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" "" { target *-*-* } .+1 } */
    y = f_g (a[5]); /* { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" } */

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
    /* { dg-message "optimized: parallelized loop nest in OpenACC .kernels. construct" "" { target *-*-* } .-1 } */
    for (int j = 0; j < N; ++j)
      b[j] = y + f_w (c[j]); /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
  }

#pragma acc kernels
  {
    y = 3; /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" } */

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
    /* { dg-message "optimized: parallelized loop nest in OpenACC .kernels. construct" "" { target *-*-* } .-1 } */
    for (int j = 0; j < N; ++j)
      b[j] = y + f_v (c[j]); /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */

    z = 2; /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" } */
  }

#pragma acc kernels /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" } */
  ;

  return 0;
}
