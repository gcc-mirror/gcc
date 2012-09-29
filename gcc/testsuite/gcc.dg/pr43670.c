/* PR debug/43670 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-vrp -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

extern void abort (void);

typedef struct { double T1; } S;

void
foo (void)
{
  int i, j;
  double s;

  S y[2][2];
  S *x[2] = { y[0], y[1] };
  S **p = x;

  for (i = 0; i < 2; i++)
    for (j = 0; j < 2; j++)
      p[j][i].T1 = 1;

  for (i = 0; i < 2; i++)
    for (j = 0; j < 2; j++)
      s = p[j][i].T1;

  if (s != 1)
    abort ();
}
