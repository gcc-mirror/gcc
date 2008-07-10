// PR middle-end/36790
// { dg-do compile }
// { dg-options "-fopenmp -O2" }

void
foo (bool b)
{
}

void
bar (bool b)
{
  foo (b);
  #pragma omp task default (shared)
    b = false;
}

int
main ()
{
  bar (false);
  return 0;
}
