// { dg-do compile }
// { dg-options "-fopenmp" }

#pragma omp declare target

int i, j;

void
f1 ()
{
  #pragma omp for linear (i:1)	// { dg-error "iteration variable .i. should not be linear" }
  for (i = 0; i < 32; i++)
    ;
}

void
f2 ()
{
  #pragma omp distribute parallel for linear (i:1)	// { dg-error ".linear. is not valid for .#pragma omp distribute parallel for." }
  for (i = 0; i < 32; i++)
    ;
}

void
f3 ()
{
  #pragma omp parallel for linear (i:1) collapse(1)
  for (i = 0; i < 32; i++)				// { dg-error "iteration variable .i. should not be linear" }
    ;
}

void
f4 ()
{
  #pragma omp for linear (i:1) linear (j:2) collapse(2)	// { dg-error "iteration variable .i. should not be linear" }
  for (i = 0; i < 32; i++)				// { dg-error "iteration variable .j. should not be linear" "" { target *-*-* } .-1 }
    for (j = 0; j < 32; j+=2)
      ;
}

void
f5 ()
{
  #pragma omp target teams distribute parallel for linear (i:1) linear (j:2) collapse(2)	// { dg-error ".linear. is not valid for .#pragma omp target teams distribute parallel for." }
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; j+=2)
      ;
}

void
f6 ()
{
  #pragma omp parallel for linear (i:1) collapse(2) linear (j:2)	// { dg-error "iteration variable .i. should not be linear" "" { target *-*-* } .+1 }
  for (i = 0; i < 32; i++)						// { dg-error "iteration variable .j. should not be linear" }
    for (j = 0; j < 32; j+=2)
      ;
}

template <int N>
void
f7 ()
{
  #pragma omp for linear (i:1)	// { dg-error "iteration variable .i. should not be linear" }
  for (i = 0; i < 32; i++)
    ;
}

template <int N>
void
f8 ()
{
  #pragma omp distribute parallel for linear (i:1)	// { dg-error ".linear. is not valid for .#pragma omp distribute parallel for." }
  for (i = 0; i < 32; i++)
    ;
}

template <int N>
void
f9 ()
{
  #pragma omp parallel for linear (i:1) collapse(1)
  for (i = 0; i < 32; i++)				// { dg-error "iteration variable .i. should not be linear" }
    ;
}

template <int N>
void
f10 ()
{
  #pragma omp for linear (i:1) linear (j:2) collapse(2)	// { dg-error "iteration variable .i. should not be linear" }
  for (i = 0; i < 32; i++)				// { dg-error "iteration variable .j. should not be linear" "" { target *-*-* } .-1 }
    for (j = 0; j < 32; j+=2)
      ;
}

template <int N>
void
f11 ()
{
  #pragma omp target teams distribute parallel for linear (i:1) linear (j:2) collapse(2)	// { dg-error ".linear. is not valid for .#pragma omp target teams distribute parallel for." }
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; j+=2)
      ;
}

template <int N>
void
f12 ()
{
  #pragma omp parallel for linear (i:1) collapse(2) linear (j:2)	// { dg-error "iteration variable .i. should not be linear" "" { target *-*-* } .+1 }
  for (i = 0; i < 32; i++)						// { dg-error "iteration variable .j. should not be linear" }
    for (j = 0; j < 32; j+=2)
      ;
}

#pragma omp end declare target

void
f13 ()
{
  f7 <0> ();
  #pragma omp target teams
  f8 <1> ();
  f9 <2> ();
  f10 <3> ();
  f11 <4> ();
  f12 <5> ();
}
