// { dg-do compile { target c++11 } }

void
foo (int x)
{
  [[omp::directive (parallel)]]
  #pragma omp for						// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  for (int i = 0; i < 16; i++)
    ;
  [[omp::directive (barrier)]]					// { dg-error "standalone OpenMP directives in 'omp::directive' attribute can only appear on an empty statement" }
  #pragma omp flush
  ;
  #pragma omp parallel
  [[omp::directive (master)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp teams
  [[omp::sequence (directive (parallel), directive (master))]]	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp task
  [[omp::directive (flush)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp master
  [[omp::directive (flush)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp for ordered
  for (int i = 0; i < 16; i++)
    #pragma omp ordered
    [[omp::directive (flush)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
    ;
  #pragma omp single
  [[omp::directive (flush)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp taskgroup
  [[omp::directive (taskyield)]]				// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp target data map (tofrom: x)
  [[omp::directive (flush)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp target
  [[omp::directive (teams)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  [[omp::directive (parallel)]]
  #pragma omp master						// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  [[omp::sequence (omp::directive (taskloop))]]			// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  for (int i = 0; i < 16; i++)
    ;
  #pragma omp parallel
  [[omp::directive (for)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  for (int i = 0; i < 16; i++)
    ;
  #pragma omp for
  [[omp::directive (master)]]					// { dg-error "for statement expected before '\\\[' token" }
  ;
  #pragma omp target teams
  [[omp::directive (parallel)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  ;
  #pragma omp parallel master
  [[omp::directive (taskloop)]]					// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  for (int i = 0; i < 16; i++)
    ;
}
