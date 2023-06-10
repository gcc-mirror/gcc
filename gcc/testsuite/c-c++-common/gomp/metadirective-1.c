/* { dg-do compile } */

#define N 100

void f (int a[], int b[], int c[])
{
  int i;

  #pragma omp metadirective \
      default (teams loop) \
      default (parallel loop) /* { dg-error "there can only be one default clause in a metadirective before '\\(' token" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      default (bad_directive) /* { dg-error "unknown directive name before '\\)' token" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      default (teams loop) \
      where (device={arch("nvptx")}: parallel loop) /* { dg-error "expected 'when' or 'default' before '\\(' token" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
      default (teams loop) \
      when (device={arch("nvptx")} parallel loop) /* { dg-error "expected colon before 'parallel'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  #pragma omp metadirective \
	default (metadirective default (flush))	/* { dg-error "metadirectives cannot be used as directive variants before 'default'" } */
    for (i = 0; i < N; i++) c[i] = a[i] * b[i];

  /* Test improperly nested metadirectives - even though the second
     metadirective resolves to 'omp nothing', that is not the same as there
     being literally nothing there.  */
  #pragma omp metadirective \
      when (implementation={vendor("gnu")}: parallel for)
    #pragma omp metadirective \
	when (implementation={vendor("cray")}: parallel for)
	/* { dg-error "loop nest expected before '#pragma'" "" { target c } .-2 } */
	/* { dg-error "'#pragma' is not allowed here" "" { target c++ } .-3 } */
      for (i = 0; i < N; i++) c[i] = a[i] * b[i];
}
