// { dg-do compile }

void foo()
{
  #pragma omp master
    {
      goto bad1;	// { dg-error "from here" }
    }

  #pragma omp master
    {
    bad1:		// { dg-error "jump|exits OpenMP" }
      return;		// { dg-error "invalid exit" }
    }
}
