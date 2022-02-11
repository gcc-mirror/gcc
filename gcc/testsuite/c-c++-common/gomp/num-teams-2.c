int fn (int);

void
foo (int i)
{
  #pragma omp teams num_teams (6 : 4)		/* { dg-warning "'num_teams' lower bound '6' bigger than upper bound '4'" } */
  ;
  #pragma omp teams num_teams (-7)		/* { dg-warning "'num_teams' value must be positive" } */
  ;
  #pragma omp teams num_teams (i : -7)		/* { dg-warning "'num_teams' value must be positive" } */
  ;
  #pragma omp teams num_teams (-7 : 8)		/* { dg-warning "'num_teams' value must be positive" } */
  ;
}

void
bar (int i)
{
  #pragma omp target teams num_teams (6 : 4)	/* { dg-warning "'num_teams' lower bound '6' bigger than upper bound '4'" } */
  ;
  #pragma omp target teams num_teams (-7)	/* { dg-warning "'num_teams' value must be positive" } */
  ;
  #pragma omp target teams num_teams (i : -7)	/* { dg-warning "'num_teams' value must be positive" } */
  ;
  #pragma omp target teams num_teams (-7 : 8)	/* { dg-warning "'num_teams' value must be positive" } */
  ;
}
