// { dg-do assemble  }

template <class T>
void f(T t1, T t2);

template <>
void f(int i, int j);

template <class T>
void g(T t1, T t2) {}

template void g(int i, int j);

void h()
{
  f(3, 'c'); // { dg-error "" } no matching function
  g(3, 'c'); // { dg-error "" } no matching function
}


