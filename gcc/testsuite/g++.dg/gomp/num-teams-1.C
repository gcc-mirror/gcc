int fn1 (int);
template <typename T>
T fn2 (T);

template <int N>
void
f1 ()
{
  #pragma omp teams num_teams (4 : 6)
  ;
  #pragma omp teams num_teams (7)
  ;
}

template <int N>
void
f2 ()
{
  #pragma omp target teams num_teams (5 : 19)
  ;
  #pragma omp target teams num_teams (21)
  ;
}

template <int N>
void
f3 ()
{
  #pragma omp teams num_teams (fn1 (1) : fn1 (2))
  ;
  #pragma omp teams num_teams (fn1 (3))
  ;
}

template <int N>
void
f4 ()
{
  #pragma omp target teams num_teams (fn1 (4) : fn1 (5))
  ;
  #pragma omp target teams num_teams (fn1 (6))
  ;
}

template <int N>
void
f5 ()
{
  #pragma omp target
  #pragma omp teams num_teams (fn1 (7) : fn1 (8))
  ;
  #pragma omp target
  #pragma omp teams num_teams (fn1 (9))
  ;
}

template <typename T, T N4, T N6, T N7>
void
f1 ()
{
  #pragma omp teams num_teams (N4 : N6)
  ;
  #pragma omp teams num_teams (N7)
  ;
}

template <typename T, T N5, T N19, T N21>
void
f2 ()
{
  #pragma omp target teams num_teams (N5 : N19)
  ;
  #pragma omp target teams num_teams (N21)
  ;
}

template <typename T, T N1, T N2, T N3>
void
f3 ()
{
  #pragma omp teams num_teams (fn2 (N1) : fn2 (N2))
  ;
  #pragma omp teams num_teams (fn2 (N3))
  ;
}

template <typename T, T N4, T N5, T N6>
void
f4 ()
{
  #pragma omp target teams num_teams (fn2 (N4) : fn2 (N5))
  ;
  #pragma omp target teams num_teams (fn2 (N6))
  ;
}

template <typename T, T N7, T N8, T N9>
void
f5 ()
{
  #pragma omp target
  #pragma omp teams num_teams (fn2 (N7) : fn2 (N8))
  ;
  #pragma omp target
  #pragma omp teams num_teams (fn2 (N9))
  ;
}

void
test ()
{
  f1<0> ();
  f2<0> ();
  f3<0> ();
  f4<0> ();
  f5<0> ();
  f1<int, 4, 6, 7> ();
  f2<int, 5, 19, 21> ();
  f3<int, 1, 2, 3> ();
  f4<int, 4, 5, 6> ();
  f5<int, 7, 8, 9> ();
}
