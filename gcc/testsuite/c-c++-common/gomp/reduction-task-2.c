int v;
extern void foo (int);

void
bar (void)
{
  int i;
  #pragma omp for reduction (task, +: v) nowait	/* { dg-error "'task' reduction modifier on a construct with a 'nowait' clause" } */
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp sections nowait reduction (task, +: v)	/* { dg-error "'task' reduction modifier on a construct with a 'nowait' clause" } */
  {
    foo (-2);
    #pragma omp section
    foo (-3);
  }
  #pragma omp simd reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'for' or 'sections'" } */
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp for simd reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct combined with 'simd'" } */
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp parallel for simd reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct combined with 'simd'" } */
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp teams distribute parallel for simd reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct combined with 'simd'" } */
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp taskloop reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'for' or 'sections'" } */
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp taskloop simd reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct combined with 'simd'" } */
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp teams reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'for' or 'sections'" } */
  foo (i);
  #pragma omp teams distribute reduction (task, +: v)	/* { dg-error "invalid 'task' reduction modifier on construct not combined with 'parallel', 'for' or 'sections'" } */
  for (i = 0; i < 64; i++)
    foo (i);
}
