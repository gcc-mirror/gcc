/* Check offloaded function's attributes and classification for OpenACC
   routine with 'nohost' clause.  */

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
#pragma acc declare copyin (a, b) create (c)

#pragma acc routine nohost worker
void ROUTINE ()
{
#pragma acc loop /* { dg-bogus "assigned OpenACC .* loop parallelism" } */
  for (unsigned int i = 0; i < N; i++)
    c[i] = a[i] + b[i];
}

/* Check the offloaded function's attributes.
   { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(omp declare target \\(nohost worker\\), oacc function \\(0 1, 1 0, 1 0\\)\\)\\)" 1 "ompexp" } } */

/* Check the offloaded function's classification.
   { dg-final { scan-tree-dump-times "(?n)Function is OpenACC routine level 1" 1 "oaccloops" } }
   { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'ROUTINE' has 'nohost' clause" 1 "oaccloops" { target c } } }
   { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'void ROUTINE\\(\\)' has 'nohost' clause" 1 "oaccloops" { target { c++ && { ! offloading_enabled } } } } }
   { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'ROUTINE\\(\\)' has 'nohost' clause" 1 "oaccloops" { target { c++ && offloading_enabled } } } }
   { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'ROUTINE' discarded" 1 "oaccloops" { target c } } }
   { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'void ROUTINE\\(\\)' discarded" 1 "oaccloops" { target { c++ && { ! offloading_enabled } } } } }
   { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'ROUTINE\\(\\)' discarded" 1 "oaccloops" { target { c++ && offloading_enabled } } } }
   TODO See PR101551 for 'offloading_enabled' differences.
   { dg-final { scan-tree-dump-not "(?n)Compute dimensions" "oaccloops" } }
   { dg-final { scan-tree-dump-not "(?n)__attribute__\\(.*omp declare target \\(nohost" "oaccloops" } }
   { dg-final { scan-tree-dump-not "(?n)void ROUTINE \\(\\)" "oaccloops" } } */
