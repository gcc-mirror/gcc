// { dg-do compile { target c++11 } }

void
foo ()
{
  // Unsure if this shouldn't be invalid, whether we shouldn't require
  // that each standalone directive sits on its own empty statement.
  [[omp::sequence (omp::directive (barrier), omp::directive (barrier))]];
  [[omp::sequence (omp::directive (taskyield), omp::directive (taskwait))]];
}
