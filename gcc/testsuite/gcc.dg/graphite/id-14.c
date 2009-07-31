typedef struct {
  int n;
  float *a;
} bar;

float
foo (bar *b)
{
  float c, d;
  int j;

  for (j = 0; (j < b->n); j++)
    d += b->a[j];

  for (j = 0; (j < b->n); j++)
    c += b->a[j];

  return d;
}
