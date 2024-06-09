// { dg-do compile { target c++11 } }

void
foo ()
{
  [[omp::decl]] int v1;						// { dg-error "'omp::decl' attribute requires argument" }
  [[omp::decl ()]] int v2;					// { dg-error "expected OpenMP directive name" }
								// { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 }
  [[omp::decl (nonexistent foobar)]] int v3;			// { dg-error "unknown OpenMP directive name in 'omp::decl' attribute argument" }
								// { dg-error "'omp::decl' not allowed to be specified in this context" "" { target *-*-* } .-1 }
  [[omp::sequence(decl(threadprivate))]] int v4;		// { dg-error "expected 'directive' or 'sequence'" }
								// { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 }
  [[omp::sequence(omp::decl(threadprivate))]] int v5;		// { dg-error "expected 'directive' or 'sequence'" }
								// { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 }
  [[omp::decl (barrier)]];					// { dg-error "OpenMP 'omp::decl' attribute on a statement" }
  [[omp::decl (parallel)]] {};					// { dg-error "OpenMP 'omp::decl' attribute on a statement" }
  extern int [[omp::decl (threadprivate)]] *v6;			// { dg-warning "attribute ignored" }
  [[omp::decl (threadprivate (v5))]] static int v7;		// { dg-error "expected end of line before '\\\(' token" }
  extern int v8;
  [[omp::decl (declare target (v8))]] static int v9;		// { dg-error "expected end of line before '\\\(' token" }
  [[omp::decl (declare target enter (v8))]] static int v10;	// { dg-error "expected an OpenMP clause before '\\\(' token" }
  [[omp::decl (declare target, link (v9))]] static int v11;	// { dg-error "expected an OpenMP clause before '\\\(' token" }
  [[omp::decl (declare target device_type (any))]] static int v12;	// { dg-error "directive with only 'device_type' or 'indirect' clauses" }
}

int i;
[[omp::decl (assume (i < 42))]];				// { dg-error "OpenMP 'omp::decl' attribute on a statement" }
