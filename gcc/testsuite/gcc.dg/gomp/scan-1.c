int baz (void);
void qux (int);
int r;

int
foo (void)
{
  int r = 0, i;
  void bar (void) { r++; }
  #pragma omp parallel for reduction(inscan, +:r)
  for (i = 0; i < 64; i++)
    {
      r += baz ();
      #pragma omp scan inclusive(r)
      qux (r);
    }
  #pragma omp parallel for reduction(inscan, +:r)
  for (i = 0; i < 64; i++)
    {
      qux (r);
      #pragma omp scan exclusive(r)
      r += baz ();
    }
  bar ();
  return r;
}

int
corge (void)
{
  int r = 0, i;
  void bar (void)
  {
    #pragma omp parallel for reduction(inscan, +:r)
    for (i = 0; i < 64; i++)
      {
	r += baz ();
	#pragma omp scan inclusive(r)
	qux (r);
      }
    #pragma omp parallel for reduction(inscan, +:r)
    for (i = 0; i < 64; i++)
      {
	qux (r);
	#pragma omp scan exclusive(r)
	r += baz ();
      }
  }
  bar ();
  return r;
}
