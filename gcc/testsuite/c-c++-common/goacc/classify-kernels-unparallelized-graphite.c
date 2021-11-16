/* Check offloaded function's attributes and classification for unparallelized
   OpenACC 'kernels' with Graphite kernles handling (default).  */

/* { dg-additional-options "-O2" }
   { dg-additional-options "-fopt-info-optimized-omp" }
   { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "-fdump-tree-ompexp" }
   { dg-additional-options "-fdump-tree-graphite-details" }
   { dg-additional-options "-fdump-tree-oaccloops1" }
   { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose-details" } */

#define N 1024

extern unsigned int *__restrict a;
extern unsigned int *__restrict b;
extern unsigned int *__restrict c;

extern unsigned int f (unsigned int);
#pragma acc routine (f) seq

void KERNELS ()
{
#pragma acc kernels copyin (a[0:N], b[0:N]) copyout (c[0:N])
  for (unsigned int i = 0; i < N; i++)
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } .-1 } */
    /* An "extern"al mapping of loop iterations/array indices makes the loop
       unparallelizable.  */
    c[i] = a[f (i)] + b[f (i)]; /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
}

/* Check the offloaded function's attributes.
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc parallel_kernels_graphite, omp target entrypoint\\)\\)" 1 "ompexp" } } */

/* Check that Graphite can handle neither the original nor the offloaded region
   { dg-final { scan-tree-dump-times "number of SCoPs: 0" 2 "graphite" } }

/* Check the offloaded function's classification and compute dimensions (will
   always be 1 x 1 x 1 for non-offloading compilation).
   { dg-final { scan-tree-dump-times "(?n)Function is parallel_kernels_graphite OpenACC kernels offload" 1 "oaccloops1" } }
   { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccloops1" } }
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(1, 1, 1\\), oacc parallel_kernels_graphite, omp target entrypoint\\)\\)" 1 "oaccloops1" } } */
