/* PR tree-optimization/99121 - ICE in -Warray-bounds on a multidimensional
   VLA
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#define NOIPA __attribute__ ((noipa))

void sink (void*, ...);
#define T(a, x) sink (a, x)


NOIPA void a_0_n (int n)
{
  int a[0][n];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_n_0 (int n)
{
  int a[n][0];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}


NOIPA void a_1_n_0 (int n)
{
  int a[1][n][0];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_1_0_n (int n)
{
  int a[1][0][n];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_0_1_n (int n)
{
  int a[0][1][n];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_0_n_1 (int n)
{
  int a[0][n][1];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_n_0_n (int n)
{
  int a[n][0][n];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_n_n_0 (int n)
{
  int a[n][n][0];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_0_n_n (int n)
{
  int a[0][n][n];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_0_0_n (int n)
{
  int a[0][0][n];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_n_0_0 (int n)
{
  int a[n][0][0];

  sink (a);

  T (a, ((int *) a)[0]);      // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((char *) a)[1]);     // { dg-warning "\\\[-Warray-bounds" }
  T (a, ((float *) a)[n]);    // { dg-warning "\\\[-Warray-bounds" }
}

NOIPA void a_n_n_n (int n)
{
  int a[n][n][n];

  sink (a);

  T (a, ((int *) a)[-1]);     // { dg-warning "\\\[-Warray-bounds" "pr99140" }
  T (a, ((int *) a)[0]);
  T (a, ((char *) a)[1]);
  T (a, ((float *) a)[n]);
}
