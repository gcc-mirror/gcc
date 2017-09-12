/* PR target/81988 */
/* Testcase by James Cowgill <jcowgill+gcc@jcowgill.uk> */

/* { dg-do assemble } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O3 -fpie" } */

int c, d;

short **e;
int *a;

void foo(void)
{
  int g[64 * 35], *h = g;
  do {
    short *f = e[d];
    for (int i = 0; i < 4; i++)
      a[i] = a[i] + (h[364] + f[4] * h[64] + f[5] * h[i] + f[6] * h[i + 3 * 4] +
                     f[7] * h[i + 4]);
  } while (c);
}
