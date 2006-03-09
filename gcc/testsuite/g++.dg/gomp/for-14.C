// { dg-do compile }

extern int printf (const char *, ...);
extern void foo (int *);

int main (void)
{
  double d = 6;
  int i = 1, j = 6, k = 8;
#pragma omp parallel shared(d) private(i) num_threads (4)
  {
    i = 4;
#pragma omp for lastprivate(j)
    for (j = 1; j <= k; j++)
      printf ("%s %d %d %d %p %g\n", "Hello, World!", i, j, k, &j, d);
    printf ("%s %d %g\n", "Hello, World!", i, d);
  }
  return 0;
}
