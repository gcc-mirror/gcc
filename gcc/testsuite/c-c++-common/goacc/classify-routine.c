/* Check offloaded function's attributes and classification for OpenACC
   routine.  */

/* { dg-additional-options "-O2" }
   { dg-additional-options "-fopt-info-optimized-omp" }
   { dg-additional-options "-fdump-tree-ompexp" }
   { dg-additional-options "-fdump-tree-oaccdevlow" } */

#define N 1024

extern unsigned int *__restrict a;
extern unsigned int *__restrict b;
extern unsigned int *__restrict c;
#pragma acc declare copyin (a, b) create (c)

#pragma acc routine worker
void ROUTINE ()
{
#pragma acc loop /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
  for (unsigned int i = 0; i < N; i++)
    c[i] = a[i] + b[i];
}

/* Check the offloaded function's attributes.
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(omp declare target \\(worker\\), oacc function \\(0 1, 1 0, 1 0\\)\\)\\)" 1 "ompexp" } } */

/* Check the offloaded function's classification and compute dimensions (will
   always be 1 x 1 x 1 for non-offloading compilation).
   { dg-final { scan-tree-dump-times "(?n)Function is OpenACC routine level 1" 1 "oaccdevlow" } }
   { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccdevlow" } }
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(0 1, 1 1, 1 1\\), omp declare target \\(worker\\), oacc function \\(0 1, 1 0, 1 0\\)\\)\\)" 1 "oaccdevlow" } } */
