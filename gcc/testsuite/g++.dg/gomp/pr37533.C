// PR c++/37533
// { dg-do compile }
// { dg-options "-fopenmp" }

template<int>
void
f1 ()
{
#pragma omp parallel for
  for (int i = ""; i < 4; ++i)	// { dg-error "invalid conversion from" }
    ;
}

template<int>
void
f2 ()
{
  int i;
#pragma omp parallel for
  for (i = ""; i < 4; ++i)	// { dg-error "invalid conversion from" }
    ;
}

template<typename T>
void
f3 ()
{
#pragma omp parallel for
  for (T i = ""; i < 4; ++i)	// { dg-error "invalid conversion from" }
    ;
}

template<typename T>
void
f4 ()
{
  T i;
#pragma omp parallel for
  for (i = ""; i < 4; ++i)	// { dg-error "invalid conversion from" }
    ;
}

void
bar ()
{
  f1<0> ();			// { dg-message "required from here" }
  f2<1> ();			// { dg-message "required from here" }
  f3<int> ();			// { dg-message "required from here" }
  f4<int> ();			// { dg-message "required from here" }
}
