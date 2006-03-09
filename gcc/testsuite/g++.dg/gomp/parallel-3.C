// { dg-do compile }

extern int printf (const char *, ...);

int main (void)
{
  double d = 6;
  int i = 1;
#pragma omp parallel shared(d) private(i) num_threads (4 + i)
  {
    i = 4;
    printf ("%s %d %g\n", "Hello, World!", i, d);
  }
  return 0;
}
