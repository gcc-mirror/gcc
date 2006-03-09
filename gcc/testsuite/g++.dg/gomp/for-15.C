// PR c++/24512
// { dg-do compile }

template<typename T> void foo ()
{
#pragma omp for
  for (int i = 0; i < 10; i++);

#pragma omp for
  for (int i = 0; i < 10; i++);

#pragma omp for
  for (T j = 0; j < 10; j++);

#pragma omp for
  for (T j = 0; j < 10; j++);

#pragma omp parallel for
  for (int k = 0; k < 10; k++);

#pragma omp parallel for
  for (int k = 0; k < 10; k++);

#pragma omp parallel for
  for (T l = 0; l < 10; l++);

#pragma omp parallel for
  for (T l = 0; l < 10; l++);
}

void bar ()
{
  foo<int> ();
  foo<long> ();
}
