// { dg-do compile }

int bar ();

void foo()
{
  int i;

  #pragma omp for schedule		// { dg-error "expected" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule static	// { dg-error "expected" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (		// { dg-error "invalid schedule kind" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static	// { dg-error "expected" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static )
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( foo )	// { dg-error "invalid schedule kind" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static 1	// { dg-error "expected" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static 1 ) nowait	// { dg-error "expected" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static, 1 ) nowait
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static, 1, 1 ) nowait  // { dg-error "expected" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static, 1 + 1 ) nowait
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule ( static, 1.0 )	// { dg-error "integral" }
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (dynamic)
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (dynamic, bar ())
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (guided)
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (guided, bar ())
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (runtime)
  for (i = 0; i < 10; ++i) ;

  #pragma omp for schedule (runtime, bar ())	// { dg-error "does not take" }
  for (i = 0; i < 10; ++i) ;
}
