template <typename T>
struct A { int foo (); int c; };

template <typename T>
int
A<T>::foo ()
{
  int j;
  #pragma omp atomic read
  j = A::c;
  return j;
}
