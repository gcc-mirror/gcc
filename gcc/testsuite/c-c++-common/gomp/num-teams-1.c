int fn (int);

void
foo (void)
{
  #pragma omp teams num_teams (4 : 6)
  ;
  #pragma omp teams num_teams (7)
  ;
}

void
bar (void)
{
  #pragma omp target teams num_teams (5 : 19)
  ;
  #pragma omp target teams num_teams (21)
  ;
}

void
baz (void)
{
  #pragma omp teams num_teams (fn (1) : fn (2))
  ;
  #pragma omp teams num_teams (fn (3))
  ;
}

void
qux (void)
{
  #pragma omp target teams num_teams (fn (4) : fn (5))
  ;
  #pragma omp target teams num_teams (fn (6))
  ;
}

void
corge (void)
{
  #pragma omp target
  #pragma omp teams num_teams (fn (7) : fn (8))
  ;
  #pragma omp target
  #pragma omp teams num_teams (fn (9))
  ;
}
