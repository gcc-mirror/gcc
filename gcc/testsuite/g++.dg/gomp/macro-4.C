// PR preprocessor/27746
// { dg-do compile }
// { dg-options "-fopenmp -Wunknown-pragmas" }

#define p		_Pragma ("omp parallel")
#define omp_p		_Pragma ("omp p") // { dg-warning "ignoring '#pragma omp _Pragma'" }

void bar (void);

void
foo (void)
{
#pragma omp p		// { dg-warning "-:ignoring '#pragma omp _Pragma'" }
    bar ();
    omp_p		// { dg-note "in expansion of macro 'omp_p'" }
    bar ();
}

#define parallel	serial
#define omp_parallel	_Pragma ("omp parallel") // { dg-warning "ignoring '#pragma omp serial'" }

void
baz (void)
{
#pragma omp parallel	// { dg-warning "-:ignoring '#pragma omp serial'" }
    bar ();
    omp_parallel        // { dg-note "in expansion of macro 'omp_parallel'" }
    bar ();
}
