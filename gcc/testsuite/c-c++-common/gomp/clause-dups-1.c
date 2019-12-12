void f0 (void);

void
f1 (int *p)
{
  int i;
  #pragma omp parallel proc_bind (master) proc_bind (master)	/* { dg-error "too many 'proc_bind' clauses" } */
  f0 ();
  #pragma omp parallel proc_bind (close) proc_bind (spread)	/* { dg-error "too many 'proc_bind' clauses" } */
  f0 ();
  #pragma omp for schedule(static) schedule(static)		/* { dg-error "too many 'schedule' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp for schedule(dynamic,5) schedule(runtime)		/* { dg-error "too many 'schedule' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp for collapse(1) collapse(1)			/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp for collapse(1) collapse(2)			/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp for ordered ordered				/* { dg-error "too many 'ordered' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp for ordered(1) ordered(1)				/* { dg-error "too many 'ordered' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp for nowait nowait					/* { dg-error "too many 'nowait' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp simd collapse(1) collapse(1)			/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp simd collapse(1) collapse(2)			/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp simd simdlen(1) simdlen(1)			/* { dg-error "too many 'simdlen' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp simd simdlen(1) simdlen(2)			/* { dg-error "too many 'simdlen' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp simd safelen(1) safelen(1)			/* { dg-error "too many 'safelen' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp simd safelen(1) safelen(2)			/* { dg-error "too many 'safelen' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp teams
  {
    #pragma omp distribute collapse(1) collapse(1)			/* { dg-error "too many 'collapse' clauses" } */
    for (i = 0; i < 8; ++i)
      f0 ();
    #pragma omp distribute collapse(1) collapse(2)			/* { dg-error "too many 'collapse' clauses" } */
    for (i = 0; i < 8; ++i)
      f0 ();
  }
  #pragma omp teams thread_limit (3) thread_limit (3)		/* { dg-error "too many 'thread_limit' clauses" } */
  f0 ();
  #pragma omp teams thread_limit (3) thread_limit (5)		/* { dg-error "too many 'thread_limit' clauses" } */
  f0 ();
  #pragma omp teams num_teams (3) num_teams (3)			/* { dg-error "too many 'num_teams' clauses" } */
  f0 ();
  #pragma omp teams num_teams (3) num_teams (5)			/* { dg-error "too many 'num_teams' clauses" } */
  f0 ();
  #pragma omp single nowait nowait				/* { dg-error "too many 'nowait' clauses" } */
  f0 ();
  #pragma omp loop bind (thread) collapse(1) collapse(3)	/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp task final (0) final (0)				/* { dg-error "too many 'final' clauses" } */
  f0 ();
  #pragma omp task final (0) final (1)				/* { dg-error "too many 'final' clauses" } */
  f0 ();
  #pragma omp task priority (1) priority (1)			/* { dg-error "too many 'priority' clauses" } */
  f0 ();
  #pragma omp task priority (0) priority (1)			/* { dg-error "too many 'priority' clauses" } */
  f0 ();
  #pragma omp taskloop final (0) final (0)			/* { dg-error "too many 'final' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop final (0) final (1)			/* { dg-error "too many 'final' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop priority (1) priority (1)		/* { dg-error "too many 'priority' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop priority (0) priority (1)		/* { dg-error "too many 'priority' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop grainsize (1) grainsize (2)		/* { dg-error "too many 'grainsize' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop grainsize (2) grainsize (2)		/* { dg-error "too many 'grainsize' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop num_tasks (1) num_tasks (2)		/* { dg-error "too many 'num_tasks' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop num_tasks (2) num_tasks (2)		/* { dg-error "too many 'num_tasks' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop num_tasks (1) grainsize (2)
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop grainsize (2) num_tasks (2)
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop collapse (1) collapse (1)		/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop collapse (1) collapse (2)		/* { dg-error "too many 'collapse' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp target data device (1) device (1) map (alloc: i)		/* { dg-error "too many 'device' clauses" } */
  f0 ();
  #pragma omp target enter data device (1) device (1) map (to: i)	/* { dg-error "too many 'device' clauses" } */
  #pragma omp target enter data nowait nowait map (to: i)		/* { dg-error "too many 'nowait' clauses" } */
  #pragma omp target exit data device (1) device (1) map (from: i)	/* { dg-error "too many 'device' clauses" } */
  #pragma omp target exit data nowait nowait map (from: i)		/* { dg-error "too many 'nowait' clauses" } */
  #pragma omp target device (1) device (1)			/* { dg-error "too many 'device' clauses" } */
  f0 ();
  #pragma omp target nowait nowait				/* { dg-error "too many 'nowait' clauses" } */
  f0 ();
  #pragma omp target update device (1) device (1) to (i)	/* { dg-error "too many 'device' clauses" } */
  #pragma omp target update nowait nowait to (i)		/* { dg-error "too many 'nowait' clauses" } */
  #pragma omp atomic seq_cst seq_cst				/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic release release				/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic relaxed relaxed				/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic seq_cst release				/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic release relaxed				/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic relaxed seq_cst				/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic hint(0) hint(0)				/* { dg-error "too many 'hint' clauses" } */
  p[0]++;
  #pragma omp atomic update seq_cst seq_cst			/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic update release release			/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic update relaxed relaxed			/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic update seq_cst release			/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic update release relaxed			/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic update relaxed seq_cst			/* { dg-error "too many memory order clauses" } */
  p[0]++;
  #pragma omp atomic update hint (0) hint(0)			/* { dg-error "too many 'hint' clauses" } */
  p[0]++;
  #pragma omp atomic write seq_cst seq_cst			/* { dg-error "too many memory order clauses" } */
  p[0] = 0;
  #pragma omp atomic write release release			/* { dg-error "too many memory order clauses" } */
  p[0] = 0;
  #pragma omp atomic write relaxed relaxed			/* { dg-error "too many memory order clauses" } */
  p[0] = 0;
  #pragma omp atomic write seq_cst release			/* { dg-error "too many memory order clauses" } */
  p[0] = 0;
  #pragma omp atomic write release relaxed			/* { dg-error "too many memory order clauses" } */
  p[0] = 0;
  #pragma omp atomic write relaxed seq_cst			/* { dg-error "too many memory order clauses" } */
  p[0] = 0;
  #pragma omp atomic write hint(0)hint(0)			/* { dg-error "too many 'hint' clauses" } */
  p[0] = 0;
  #pragma omp atomic read seq_cst seq_cst			/* { dg-error "too many memory order clauses" } */
  i = p[0];
  #pragma omp atomic read acquire acquire			/* { dg-error "too many memory order clauses" } */
  i = p[0];
  #pragma omp atomic read relaxed relaxed			/* { dg-error "too many memory order clauses" } */
  i = p[0];
  #pragma omp atomic read seq_cst acquire			/* { dg-error "too many memory order clauses" } */
  i = p[0];
  #pragma omp atomic read acquire relaxed			/* { dg-error "too many memory order clauses" } */
  i = p[0];
  #pragma omp atomic read relaxed seq_cst			/* { dg-error "too many memory order clauses" } */
  i = p[0];
  #pragma omp atomic read hint (0) hint(0)			/* { dg-error "too many 'hint' clauses" } */
  i = p[0];
  #pragma omp atomic capture seq_cst seq_cst			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture acq_rel acq_rel			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture acquire acquire			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture release release			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture relaxed relaxed			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture seq_cst acq_rel			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture acq_rel acquire			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture acquire release			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture release relaxed			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture relaxed seq_cst			/* { dg-error "too many memory order clauses" } */
  i = p[0]++;
  #pragma omp atomic capture hint(0) hint (0)			/* { dg-error "too many 'hint' clauses" } */
  i = p[0]++;
  
}

#pragma omp declare simd simdlen (4) simdlen (4)		/* { dg-error "too many 'simdlen' clauses" } */
void f2 (int a, int b);
#pragma omp declare simd simdlen (4) simdlen (8)		/* { dg-error "too many 'simdlen' clauses" } */
void f3 (int a, int b);
#pragma omp declare simd uniform (a) uniform (a)		/* { dg-error "'a' appears more than once in data clauses" } */
void f4 (int a, int b);
#pragma omp declare simd linear (a) linear (a)			/* { dg-error "'a' appears more than once in data clauses" } */
void f5 (int a, int b);
#pragma omp declare simd linear (a) linear (a:3)		/* { dg-error "'a' appears more than once in data clauses" } */
void f6 (int a, int b);
#pragma omp declare simd uniform (a) linear (a)			/* { dg-error "'a' appears more than once in data clauses" } */
void f7 (int a, int b);
#pragma omp declare simd linear (a) uniform (a)			/* { dg-error "'a' appears more than once in data clauses" } */
void f8 (int a, int b);
