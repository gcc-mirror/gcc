/* PR c/65467 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c11" } */

_Atomic int t;
#pragma omp threadprivate (t)

void
foo (void)
{
  _Atomic int a = 4, b = 0, c, d = 3, e;
  a++;
  #pragma omp parallel sections num_threads (a) shared (b) private (c) firstprivate (d) lastprivate (e)
  {
  #pragma omp section
    {
      a++;
      b++;
      c = 5;
      c++;
      d++;
      e = 9;
      e++;
    }
  #pragma omp section
    {
      a++;
      b++;
      c = 5;
      c++;
      d++;
      e = 3;
      e++;
    }
  }
  e++;
  t++;
  #pragma omp parallel copyin (t) private (e)
  {
    t++;
    e = t;
    #pragma omp single copyprivate (e)
    {
      e++;
    }
    e++;
  }
}

void
bar (void)
{
  int a[4];
  _Atomic int b = 1, c = 2, f = 8, g = 8, h = 0;
  _Atomic int d, e[3];
  int *_Atomic p;
  _Atomic int *_Atomic q;
  int i, j;
  p = a;
  q = e;
  #pragma omp target teams map (tofrom: a[b:c]) num_teams (b) thread_limit (c)
  a[1]++;
  #pragma omp target device(h)
  ;
  #pragma omp task depend (inout: a[b:c])
  ;
  #pragma omp task depend (out: d, e[b:c]) priority (b)
  ;
  #pragma omp task depend (out: p[b:c])
  ;
  #pragma omp task depend (out: q[b:c])
  ;
  #pragma omp taskloop num_tasks (c)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp taskloop grainsize (c)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp parallel for schedule (dynamic, b)
  for (i = 0; i < 16; i++)
    ;
  j = 0;
  #pragma omp simd linear(j:b)
  for (i = 0; i < 16; i++)
    j += b;
  j = 4;
  #pragma omp atomic read
  b = j;
  #pragma omp atomic write
  j = c;
  #pragma omp atomic
  j += c;
  #pragma omp atomic capture
  b = j += c;
  #pragma omp atomic capture
  b = ++j;
  #pragma omp atomic capture
  { b = j; j = c; }
  #pragma omp atomic capture
  { b = j; j++; }
  #pragma omp atomic capture
  { j *= c; b = j; }
}
