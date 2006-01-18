// { dg-do compile }

void foo()
{
  #pragma omp master
    {
      goto bad1;	// { dg-error "invalid exit" }
    }

  #pragma omp master
    {
    bad1:
      return;		// { dg-error "invalid exit" }
    }
}
