#ifdef __cplusplus
extern "C" {
#endif

int omp_get_num_teams (void);
int omp_get_team_num (void);

#ifdef __cplusplus
}
#endif

void bar (int *, int *, int *, int, int, int, int);

void
foo (void)
{
  int a = 1, b = 2, c = 3, d = 4, e = 5, f = 6;
  #pragma omp teams num_teams (4) shared (b) firstprivate (c, d) private (e, f)
  {
    f = 7;
    bar (&a, &c, &e, b, d, f, 0);
  }
  bar (&a, (int *) 0, (int *) 0, b, 0, 0, 1);
}

void
baz (void)
{
  #pragma omp teams
  {
    #pragma omp distribute
    for (int i = 0; i < 64; i++)
      ;
    #pragma omp distribute simd
    for (int i = 0; i < 64; i++)
      ;
    #pragma omp distribute parallel for
    for (int i = 0; i < 64; i++)
      ;
    #pragma omp distribute parallel for
    for (int i = 0; i < 64; i++)
      ;
    #pragma omp distribute parallel for simd
    for (int i = 0; i < 64; i++)
      ;
    #pragma omp parallel
    ;
    #pragma omp parallel for
    for (int i = 0; i < 64; i++)
      ;
    #pragma omp parallel for simd
    for (int i = 0; i < 64; i++)
      ;
    int a, b;
    #pragma omp parallel sections
    {
      a = 5;
      #pragma omp section
      b = 6;
    }
    int c = omp_get_num_teams ();
    int d = omp_get_team_num ();
  }
}
