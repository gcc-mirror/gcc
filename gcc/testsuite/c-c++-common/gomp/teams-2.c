void
foo (void)
{
  int i;

  #pragma omp parallel
  {
    #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
    ;
  }
  #pragma omp teams
  {
    #pragma omp teams	/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
  }
  #pragma omp target
  {
    #pragma omp parallel
    {
      #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
      ;
    }
  }
  #pragma omp for
  for (i = 0; i < 4; i++)
    if (i == 0)
      {
	#pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
	;
      }
  #pragma omp single
  #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
  ;
  #pragma omp master
  {
    #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
    ;
  }
  #pragma omp critical
  #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
  ;
  #pragma omp sections
  {
    #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
    ;
    #pragma omp section
    {
      #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
      ;
    }
  }
  #pragma omp target data map (to: i)
  {
    #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
    ;
  }
  #pragma omp task
  {
    #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
    ;
  }
  #pragma omp taskgroup
  {
    #pragma omp teams	/* { dg-error "'teams' construct must be closely nested inside of 'target' construct or not nested in any OpenMP construct" } */
    ;
  }
}

void
bar (void)
{
  #pragma omp teams
  {
    int x, y, v = 4;
    #pragma omp target			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp target data map (to: v)	/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp for			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    for (int i = 0; i < 64; ++i)
      ;
    #pragma omp simd			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    for (int i = 0; i < 64; ++i)
      ;
    #pragma omp for simd		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    for (int i = 0; i < 64; ++i)
      ;
    #pragma omp single			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp master			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp sections		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    {
      x = 1;
      #pragma omp section
      y = 2;
    }
    #pragma omp critical		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp target enter data map (to: v)	/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    #pragma omp target exit data map (from: v)	/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    #pragma omp cancel parallel		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    #pragma omp cancellation point parallel /* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    #pragma omp barrier			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    #pragma omp ordered			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp task			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp taskloop		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    for (int i = 0; i < 64; ++i)
      ;
    #pragma omp atomic			/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    v++;
    #pragma omp taskgroup		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    ;
    #pragma omp taskwait		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
    #pragma omp taskyield		/* { dg-error "only 'distribute' or 'parallel' regions are allowed to be strictly nested inside 'teams' region" } */
  }
}
