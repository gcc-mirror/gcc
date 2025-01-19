/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-fopenmp -std=c23" { target { c } } } */

#define N 100

void
f (int a[], int b[], int c[])
{
  int i;

  [[omp::directive (metadirective
      default (teams loop)
      default (parallel loop))]] /* { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  [[omp::directive (metadirective
      default (bad_directive))]] /* { dg-error "unknown directive name before '\\)' token" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  [[omp::directive (metadirective
      where (device={arch("nvptx")}: parallel loop) /* { dg-error "'where' is not valid for 'metadirective'" } */
      default (teams loop))]]
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  [[omp::directive (metadirective
      otherwise (teams loop)
      when (device={arch("nvptx")}: parallel loop))]] /* { dg-error "'otherwise' or 'default' clause must appear last" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];
  
  [[omp::directive (metadirective
      when (device={arch("nvptx")} parallel loop) /* { dg-error "expected ':' before 'parallel'" } */
      default (teams loop))]]
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  [[omp::directive (metadirective
      default (metadirective default (flush)))]]	/* { dg-error "metadirectives cannot be used as variants of a 'metadirective' before 'default'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  /* Test improperly nested metadirectives - even though the second
     metadirective resolves to 'omp nothing', that is not the same as there
     being literally nothing there.  */
  [[omp::directive (metadirective
      when (implementation={vendor("gnu")}: parallel for))]]
  [[omp::directive (metadirective      /* { dg-error "loop nest expected" } */
      when (implementation={vendor("cray")}: parallel for))]]
      for (i = 0; i < N; i++) c[i] = a[i] * b[i];
}
