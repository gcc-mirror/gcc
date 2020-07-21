// PR96257 we scan forwards checking for a compound literal.  Do not
// eat the tokens when doing that!
/* { dg-require-effective-target fopenmp } */
/* { dg-additional-options -fopenmp }  */

int
f2 ()
{
  int s = (int // { dg-error "expected" }
#pragma omp atomic capture
       ){1};

  int t = (int // { dg-error "expected" }
#pragma omp atomic capture ){
    {1};

    return s + t;
} // { dg-bogus "expected" }
