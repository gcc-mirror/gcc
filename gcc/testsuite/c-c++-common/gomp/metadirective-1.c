/* { dg-do compile } */

#define N 100

void
f (int a[], int b[], int c[])
{
  int i;

  #pragma omp metadirective \
      default (teams loop) \
      default (parallel loop) /* { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      otherwise (teams loop) \
      default (parallel loop) /* { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      otherwise (teams loop) \
      otherwise (parallel loop) /* { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      default (bad_directive) /* { dg-error "unknown directive name before '\\)' token" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      where (device={arch("nvptx")}: parallel loop) /* { dg-error "'where' is not valid for 'metadirective'" } */ \
      default (teams loop)
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      otherwise (teams loop) \
      when (device={arch("nvptx")}: parallel loop) /* { dg-error "'otherwise' or 'default' clause must appear last" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];
    
  #pragma omp metadirective \
      when (device={arch("nvptx")} parallel loop) /* { dg-error "expected ':' before 'parallel'" } */ \
      default (teams loop)
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
	default (metadirective default (flush))	/* { dg-error "metadirectives cannot be used as variants of a 'metadirective' before 'default'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  /* Test improperly nested metadirectives - even though the second
     metadirective resolves to 'omp nothing', that is not the same as there
     being literally nothing there.  */
  #pragma omp metadirective \
      when (implementation={vendor("gnu")}: parallel for)
    #pragma omp metadirective \
	when (implementation={vendor("cray")}: parallel for)
	/* { dg-error "loop nest expected before '#pragma'" "" { target c } .-2 } */
	/* { dg-error "loop nest expected" "" { target c++ } .-3 } */
      for (i = 0; i < N; i++) c[i] = a[i] * b[i];
}
