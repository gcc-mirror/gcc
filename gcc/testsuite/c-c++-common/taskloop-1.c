/* { dg-do compile } */

int e;
int bar (int, int);
void baz (int, int, int, int *, int *, int *);

void
foo (int a, int b, int c, int d, int f, int g, int h, int j, int k, int l)
{
  int i;
  #pragma omp taskloop if (a) final (b) untied default(none) mergeable \
    private(c) firstprivate (e) shared (d) num_tasks(f) collapse(1)
  for (i = bar (g, h) + j; i < k; i += l)
    baz (i, d, e++, &c, &d, &e);
}
