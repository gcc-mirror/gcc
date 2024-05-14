// { dg-do compile { target c++11 } }

#define N 100

void f (int a[], int b[], int c[])
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
      default (teams loop)
		    where (device={arch("nvptx")}: parallel loop))]] /* { dg-error "'where' is not valid for 'metadirective'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  [[omp::directive (metadirective
      default (teams loop)
      when (device={arch("nvptx")} parallel loop))]] /* { dg-error "expected ':' before 'parallel'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  [[omp::directive (metadirective
      default (metadirective default (flush)))]]	/* { dg-error "metadirectives cannot be used as variants of a 'metadirective' before 'default'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  /* Test improperly nested metadirectives - even though the second
     metadirective resolves to 'omp nothing', that is not the same as there
     being literally nothing there.  */
  [[omp::directive (metadirective
      when (implementation={vendor("gnu")}: parallel for))]]
  [[omp::directive (metadirective      /* { dg-error "'#pragma' is not allowed here" } */
      when (implementation={vendor("cray")}: parallel for))]]
      for (i = 0; i < N; i++) c[i] = a[i] * b[i];
}
