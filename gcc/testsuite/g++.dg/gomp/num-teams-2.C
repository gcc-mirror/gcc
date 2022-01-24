template <int N>
void
foo (int i)
{
  #pragma omp teams num_teams (6 : 4)		// { dg-warning "'num_teams' lower bound '6' bigger than upper bound '4'" }
  ;
  #pragma omp teams num_teams (-7)		// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp teams num_teams (i : -7)		// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp teams num_teams (-7 : 8)		// { dg-warning "'num_teams' value must be positive" }
  ;
}

template <int N>
void
bar (int i)
{
  #pragma omp target teams num_teams (6 : 4)	// { dg-warning "'num_teams' lower bound '6' bigger than upper bound '4'" }
  ;
  #pragma omp target teams num_teams (-7)	// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp target teams num_teams (i : -7)	// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp target teams num_teams (-7 : 8)	// { dg-warning "'num_teams' value must be positive" }
  ;
}

template <typename T, T NM7, T N4, T N6, T N8>
void
baz (T i)
{
  #pragma omp teams num_teams (N6 : N4)		// { dg-warning "'num_teams' lower bound '6' bigger than upper bound '4'" }
  ;
  #pragma omp teams num_teams (NM7)		// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp teams num_teams (i : NM7)		// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp teams num_teams (NM7 : N8)	// { dg-warning "'num_teams' value must be positive" }
  ;
}

template <typename T, T NM7, T N4, T N6, T N8>
void
qux (T i)
{
  #pragma omp target teams num_teams (N6 : N4)	// { dg-warning "'num_teams' lower bound '6' bigger than upper bound '4'" }
  ;
  #pragma omp target teams num_teams (NM7)	// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp target teams num_teams (i : NM7)	// { dg-warning "'num_teams' value must be positive" }
  ;
  #pragma omp target teams num_teams (NM7 : N8)	// { dg-warning "'num_teams' value must be positive" }
  ;
}

void
test ()
{
  foo<0> (5);
  bar<0> (5);
  baz<int, -7, 4, 6, 8> (5);
  qux<int, -7, 4, 6, 8> (5);
}
