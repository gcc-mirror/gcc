void f0 (void);

void
f1 (int *p)
{
  int i;
  #pragma omp task if (0) if (0)		/* { dg-error "too many 'if' clauses without modifier" } */
  f0 ();
  #pragma omp task if (0) if (1)		/* { dg-error "too many 'if' clauses without modifier" } */
  f0 ();
  #pragma omp task if (task:0) if (task:0)	/* { dg-error "too many 'if' clauses with 'task' modifier" } */
  f0 ();
  #pragma omp task if (task:0) if (1)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  f0 ();
  #pragma omp task if (0) if (task:1)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  f0 ();
  #pragma omp taskloop if (0) if (0)		/* { dg-error "too many 'if' clauses without modifier" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop if (0) if (1)		/* { dg-error "too many 'if' clauses without modifier" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop if (taskloop:0) if (taskloop:0)	/* { dg-error "too many 'if' clauses with 'taskloop' modifier" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop if (taskloop:0) if (1)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp taskloop if (0) if (taskloop:0)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp target data if (1) if (1) map (alloc: i)		/* { dg-error "too many 'if' clauses without modifier" } */
  f0 ();
  #pragma omp target data if (target data: 1) if (target data:0) map (alloc: i)	/* { dg-error "too many 'if' clauses with 'target data' modifier" } */
  f0 ();
  #pragma omp target data if (1) if (target data:0) map (alloc: i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  f0 ();
  #pragma omp target data if (target data: 1) if (0) map (alloc: i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  f0 ();
  #pragma omp target enter data if (1) if (1) map (to: i)		/* { dg-error "too many 'if' clauses without modifier" } */
  #pragma omp target enter data if (target enter data: 1) if (target enter data:0) map (to: i)	/* { dg-error "too many 'if' clauses with 'target enter data' modifier" } */
  #pragma omp target enter data if (1) if (target enter data:0) map (to: i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  #pragma omp target enter data if (target enter data: 1) if (0) map (to: i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  #pragma omp target exit data if (1) if (1) map (from: i)		/* { dg-error "too many 'if' clauses without modifier" } */
  #pragma omp target exit data if (target exit data: 1) if (target exit data:0) map (from: i)	/* { dg-error "too many 'if' clauses with 'target exit data' modifier" } */
  #pragma omp target exit data if (1) if (target exit data:0) map (from: i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  #pragma omp target exit data if (target exit data: 1) if (0) map (from: i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  #pragma omp target if (1) if (1)		/* { dg-error "too many 'if' clauses without modifier" } */
  f0 ();
  #pragma omp target if (target: 1) if (target:0)	/* { dg-error "too many 'if' clauses with 'target' modifier" } */
  f0 ();
  #pragma omp target if (1) if (target:0)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  f0 ();
  #pragma omp target if (target: 1) if (0)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  f0 ();
  #pragma omp target update if (1) if (1) to (i)		/* { dg-error "too many 'if' clauses without modifier" } */
  #pragma omp target update if (target update: 1) if (target update:0) to (i)	/* { dg-error "too many 'if' clauses with 'target update' modifier" } */
  #pragma omp target update if (1) if (target update:0) to (i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
  #pragma omp target update if (target update: 1) if (0) to (i)	/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
}
