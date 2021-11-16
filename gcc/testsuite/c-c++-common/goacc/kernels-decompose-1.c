/* Test OpenACC 'kernels' construct decomposition.  */
/* { dg-additional-options "-fopt-info-optimized-omp" } */
/* { dg-additional-options "-O2" } for "Graphite".  */

/* { dg-additional-options "-fopt-info-omp-all" } */

/* { dg-additional-options "-fdump-tree-gimple" } */

/* { dg-additional-options "--param=openacc-kernels=decompose" }
   { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose-details" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

/* See also '../../gfortran.dg/goacc/kernels-decompose-1.f95'.  */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop_i 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#define N 1024

unsigned int a[N];

int
main (void)
{
  int i;
  unsigned int sum = 1;

#pragma acc kernels copyin(a[0:N]) copy(sum) /* { dg-line l_kernels_pragma } */
  /* { dg-missed {'map\(tofrom:sum \[len: [0-9]+\]\)' not optimized: sum is unsuitable for privatization} "TODO Missing synthetic reduction clause" { target *-*-* } l_kernels_pragma } */
  {
    #pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < N; ++i)
      sum += a[i];

    sum++;
    a[0]++;

    #pragma acc loop independent /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" {target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-warning {loop has "independent" clause but data dependences were found.} "" { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < N; ++i)
      sum += a[i];

    if (sum > 10)
      { 
        #pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
	/* { dg-missed "unparallelized loop nest in OpenACC 'kernels' region: it's executed conditionally" "" { target *-*-* } l_loop_i$c_loop_i } */
	/* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
        for (i = 0; i < N; ++i)
          sum += a[i];
      }

    #pragma acc loop auto /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < N; ++i)
      sum += a[i];
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_kernels map\(tofrom:sum \[len: [0-9]+\]\) map\(to:a\[0\] \[len: [0-9]+\]\) map\(firstprivate:a \[pointer assign, bias: 0\]\)$} 1 "gimple" } }

   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(i\)$} 2 "gimple" } }
   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop independent private\(i\)$} 1 "gimple" } }
   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop auto private\(i\)$} 1 "gimple" } }
   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop} 4 "gimple" } } */

/* Check that the OpenACC 'kernels' got decomposed into 'data' and an enclosed
   sequence of compute constructs.
   { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_data_kernels map\(tofrom:sum \[len: [0-9]+\]\) map\(to:a\[0\] \[len: [0-9]+\]\)$} 1 "omp_oacc_kernels_decompose" } }
   As noted above, we get three "old-style" kernel regions, one gang-single region, and one parallelized loop region.
   { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel_kernels_graphite async\(-1\) map\(force_present:sum \[len: [0-9]+\]\) map\(force_present:a\[0\] \[len: [0-9]+\]\) map\(firstprivate:a \[pointer assign, bias: 0\]\)$} 5 "omp_oacc_kernels_decompose" } }

   'data' plus five CCs.
   { dg-final { scan-tree-dump-times {(?n)#pragma omp target } 6 "omp_oacc_kernels_decompose" } }

   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(i\)$} 1 "omp_oacc_kernels_decompose" } }
   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop independent private\(i\)$} 1 "omp_oacc_kernels_decompose" } }
   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop auto private\(i\)$} 2 "omp_oacc_kernels_decompose" } }
   { dg-final { scan-tree-dump-times {(?n)#pragma acc loop} 4 "omp_oacc_kernels_decompose" } }

   Each of the parallel regions is async, and there is a final call to
   __builtin_GOACC_wait.
   { dg-final { scan-tree-dump-times "__builtin_GOACC_wait" 1 "omp_oacc_kernels_decompose" } } */

