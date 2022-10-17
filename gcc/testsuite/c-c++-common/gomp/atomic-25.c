/* { dg-do compile } */

int x, r, z;
double d, v;
long double ld;

void
foo (int y, double e, long double f)
{
  #pragma omp atomic compare update seq_cst
  x = x > y ? y : x;
  #pragma omp atomic compare relaxed
  d = e > d ? e : d;
  #pragma omp atomic compare
  d = f < d ? f : d;
  #pragma omp atomic compare seq_cst fail(relaxed)
  x = 12U < x ? 12U : x;
  #pragma omp atomic compare
  x = x == 7 ? 24 : x;
  #pragma omp atomic compare
  x = x == 123UL ? 256LL : x;
  #pragma omp atomic compare
  ld = ld == f ? f + 5.0L : ld;
  #pragma omp atomic compare
  if (x == 9) { x = 5; }
  #pragma omp atomic compare
  if (x > 5) { x = 5; }
  #pragma omp atomic compare
  if (7 > x) { x = 7; }
  #pragma omp atomic compare update capture seq_cst fail(acquire)
  v = d = f > d ? f : d;
  #pragma omp atomic update capture compare
  v = x = x < 24ULL ? 24ULL : x;
  #pragma omp atomic compare, capture, update
  v = x = x == e ? f : x;
  #pragma omp atomic capture compare
  { v = d; if (d > e) { d = e; } }
  #pragma omp atomic compare capture
  { if (e < d) { d = e; } v = d; }
  #pragma omp atomic compare capture
  { y = x; if (x == 42) { x = 7; } }
  #pragma omp atomic capture compare weak
  { if (x == 42) { x = 7; } y = x; }
  #pragma omp atomic capture compare fail(seq_cst)
  if (d == 8.0) { d = 16.0; } else { v = d; }
  #pragma omp atomic capture compare
  { r = x == 8; if (r) { x = 24; } }
  #pragma omp atomic compare capture
  { r = x == y; if (r) { x = y + 6; } else { z = x; } }
}
