/* PR target/99085 */
/* { dg-do compile } */
/* { dg-options "-O2 -fgraphite-identity -fsel-sched-pipelining -fselective-scheduling2" } */

void
foo (int m, int n, int o, int i)
{
  long double a2[m];
  int c2[n][o];
  int j, k;

  while (i < m)
    a2[i++] = 13.132L;

  for (j = 0; j < n; j++)
    for (k = 0; k < o; k++)
      c2[j][k] = 1;

  __builtin_abort ();
}
