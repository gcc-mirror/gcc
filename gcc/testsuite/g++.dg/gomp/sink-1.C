/* { dg-do compile } */

void bar (int, int, int);

template<typename T>
void baz ()
{
  T i, j;
#pragma omp parallel for ordered(2)
  for (i=0; i < 100; ++i)
    for (j=0; j < 100; ++j)
      {
#pragma omp ordered depend(sink:i-3,j)
	bar (i, j, 0);
#pragma omp ordered depend(source)
      }
}

int main()
{
  baz<int>();
}
