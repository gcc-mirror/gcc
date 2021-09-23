// { dg-do compile { target c++11 } }

int i;
int t1, t2, t3, t4, t5, t6, t7;

void
foo ()
{
  [[omp::directive]];		// { dg-error "'omp::directive' attribute requires argument" }
  [[omp::directive ()]];	// { dg-error "expected OpenMP directive name" }
  [[omp::directive (nonexistent foobar)]];	// { dg-error "unknown OpenMP directive name in 'omp::directive' attribute argument" }
  [[omp::sequence]];		// { dg-error "'omp::sequence' attribute requires argument" }
  [[omp::sequence()]];		// { dg-error "expected 'directive' or 'sequence'" }
  [[omp::sequence(foobar())]];		// { dg-error "expected 'directive' or 'sequence'" }
  [[omp::sequence(omp::foobar())]];		// { dg-error "expected 'directive' or 'sequence'" }
  [[omp::sequence(directive(taskwait), foobar())]];		// { dg-error "expected 'directive' or 'sequence'" }
  [[omp::sequence(omp::directive(taskwait), omp::foobar())]];	// { dg-error "expected 'directive' or 'sequence'" }
  [[omp::sequence(directive(taskwait) foobar())]];		// { dg-error "expected '\\\)' before 'foobar'" }
  [[omp::sequence(directive)]];		// { dg-error "expected '\\\(' before '\\\)' token" }
  [[omp::sequence(omp::sequence)]];	// { dg-error "expected '\\\(' before '\\\)' token" }
  [[omp::directive (parallel), omp::directive (single)]]	// { dg-error "OpenMP construct among 'omp::directive' attributes requires all 'omp::directive' attributes on the same statement to be in the same 'omp::sequence'" }
    ;
  [[omp::directive (parallel)]]	// { dg-error "OpenMP construct among 'omp::directive' attributes requires all 'omp::directive' attributes on the same statement to be in the same 'omp::sequence'" }
  [[omp::directive (single)]]
    ;
  [[omp::directive (taskwait), omp::directive (taskyield)]]	// { dg-error "multiple OpenMP standalone directives among 'omp::directive' attributes must be all within the same 'omp::sequence'" }
    ;
  [[omp::directive (taskwait)]]
  [[omp::directive (taskyield)]]	// { dg-error "multiple OpenMP standalone directives among 'omp::directive' attributes must be all within the same 'omp::sequence'" }
    ;
  [[omp::directive (flush)]]	// { dg-error "standalone OpenMP directives in 'omp::directive' attribute can only appear on an empty statement" }
    i++;
  auto a = [] () [[omp::directive (threadprivate (t1))]] {};	// { dg-error "'omp::directive' not allowed to be specified in this context" }
  int [[omp::directive (threadprivate (t2))]] b;		// { dg-warning "attribute ignored" }
  int *[[omp::directive (threadprivate (t3))]] c;		// { dg-warning "'omp::directive' scoped attribute directive ignored" }
  int &[[omp::directive (threadprivate (t4))]] d = b;		// { dg-warning "'omp::directive' scoped attribute directive ignored" }
  typedef int T [[omp::directive (threadprivate (t5))]];	// { dg-error "'omp::directive' not allowed to be specified in this context" }
  int e[10] [[omp::directive (threadprivate (t6))]];		// { dg-error "'omp::directive' not allowed to be specified in this context" }
  struct [[omp::directive (threadprivate (t7))]] S {};		// { dg-error "'omp::directive' not allowed to be specified in this context" }
}
