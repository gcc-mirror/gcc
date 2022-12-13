// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

int v = 6;
void bar (int);
void bar2 (int, long *, long *);
int baz (void);
#pragma omp declare target to (bar, baz, v)

template <int N>
void
foo (int a, int b, long c, long d)
{
  /* The OpenMP 4.5 spec says that these expressions are evaluated before
     target region on combined target teams, so those cases are always
     fine.  */
  #pragma omp target
  bar (0);
  #pragma omp target
  #pragma omp teams
  bar (1);
  #pragma omp target teams
  bar (2);
  #pragma omp target
  #pragma omp teams num_teams (4)
  bar (3);
  #pragma omp target teams num_teams (4)
  bar (4);
  #pragma omp target
  #pragma omp teams thread_limit (7)
  bar (5);
  #pragma omp target teams thread_limit (7)
  bar (6);
  #pragma omp target
  #pragma omp teams num_teams (4) thread_limit (8)
  {
    {
      bar (7);
    }
  }
  #pragma omp target teams num_teams (4) thread_limit (8)
  bar (8);
  #pragma omp target
  #pragma omp teams num_teams (a) thread_limit (b)
  bar (9);
  #pragma omp target teams num_teams (a) thread_limit (b)
  bar (10);
  #pragma omp target
  #pragma omp teams num_teams (c + 1) thread_limit (d - 1)
  bar (11);
  #pragma omp target teams num_teams (c + 1) thread_limit (d - 1)
  bar (12);
  #pragma omp target map (always, to: c, d)
  #pragma omp teams num_teams (c + 1) thread_limit (d - 1)
  bar (13);
  #pragma omp target data map (to: c, d)
  {
    #pragma omp target defaultmap (tofrom: scalar)
    bar2 (14, &c, &d);
    /* This is one of the cases which can't be generally optimized,
       the c and d are (or could be) already mapped and whether
       their device and original values match is unclear.  */
    #pragma omp target map (to: c, d)
    #pragma omp teams num_teams (c + 1) thread_limit (d - 1)
    bar (15);
  }
  /* This can't be optimized, there are function calls inside of
     target involved.  */
  #pragma omp target
  #pragma omp teams num_teams (baz () + 1) thread_limit (baz () - 1)
  bar (16);
  #pragma omp target teams num_teams (baz () + 1) thread_limit (baz () - 1)
  bar (17);
  /* This one can't be optimized, as v might have different value between
     host and target.  */
  #pragma omp target
  #pragma omp teams num_teams (v + 1) thread_limit (v - 1)
  bar (18);
}

void
foo (int a, int b, long c, long d)
{
  foo<0> (a, b, c, d);
}

/* { dg-final { scan-tree-dump-times "num_teams\\(-1\\)" 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "thread_limit\\(-1\\)" 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "num_teams\\(0\\)" 4 "gimple" } } */
/* { dg-final { scan-tree-dump-times "thread_limit\\(0\\)" 6 "gimple" } } */
/* { dg-final { scan-tree-dump-times "num_teams\\(-2\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "thread_limit\\(1\\)" 0 "gimple" } } */
