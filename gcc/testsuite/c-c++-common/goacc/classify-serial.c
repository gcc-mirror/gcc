/* Check offloaded function's attributes and classification for OpenACC
   serial.  */

/* { dg-additional-options "-O2" }
   { dg-additional-options "-fopt-info-optimized-omp" }
   { dg-additional-options "-fdump-tree-ompexp" }
   { dg-additional-options "-fdump-tree-oaccloops" } */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

#define N 1024

extern unsigned int *__restrict a;
extern unsigned int *__restrict b;
extern unsigned int *__restrict c;

void SERIAL ()
{
#pragma acc serial loop copyin (a[0:N], b[0:N]) copyout (c[0:N]) /* { dg-message "optimized: assigned OpenACC gang vector loop parallelism" } */
  /* { dg-bogus "warning: region contains gang partitioned code but is not gang partitioned" "TODO 'serial'" { xfail *-*-* } .-1 }
     { dg-bogus "warning: region contains worker partitioned code but is not worker partitioned" "" { target *-*-* } .-2 }
     { dg-bogus "warning: region contains vector partitioned code but is not vector partitioned" "TODO 'serial'" { xfail *-*-* } .-3 }
     TODO Should we really diagnose this if the user explicitly requested 'serial'?
     TODO Should we instead diagnose ('-Wextra' category?) that the user may enable use of parallelism if replacing 'serial' with 'parallel', if applicable?  */
  for (unsigned int i = 0; i < N; i++)
    c[i] = a[i] + b[i];
}

/* Check the offloaded function's attributes.
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc serial, omp target entrypoint\\)\\)" 1 "ompexp" } } */

/* Check the offloaded function's classification and compute dimensions (will
   always be 1 x 1 x 1 for non-offloading compilation).
   { dg-final { scan-tree-dump-times "(?n)Function is OpenACC serial offload" 1 "oaccloops" } }
   { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccloops" } }
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(1, 1, 1\\), oacc serial, omp target entrypoint\\)\\)" 1 "oaccloops" } } */
