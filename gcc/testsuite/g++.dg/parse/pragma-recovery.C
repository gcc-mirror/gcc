/* { dg-additional-options -fopenmp }  */
/* { dg-require-effective-target fopenmp } */

// Make sure error recovery doesn't get confused by tokens inside a
// deferred pragma.
// OpenMP is a convenient deferred pragma insertion mechanism.

void foo  ()
{
  1 * "" // { dg-error "invalid" }
#pragma omp atomic {
    ;
    
    }

void bar  ()
{
  1 * "" // { dg-error "invalid" }
#pragma omp atomic }
    ;
    
    }

void baz  ()
{
  1 * "" // { dg-error "invalid" }
#pragma omp atomic ;
    0;
  
    
    }

