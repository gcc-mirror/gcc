// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

const unsigned long arr[10] = { 2 };
template<class T> struct S { int n; };

template <class T>
int fn1 (S<T>* s)
{
  int i = 1;
  return s->n < arr[i + 1];
}

template <class T>
int fn2 (S<T> s)
{
  int i = 1;
  return s.n < arr[i + 1];
}

template <class T>
int fn3 (S<T>* s)
{
  int i = 1;
  return s->template n < 1; // { dg-error "parse error in template argument list" }
}

template <class T>
int fn4 (S<T> s)
{
  int i = 1;
  return s.template n < 1; // { dg-error "parse error in template argument list" }
}
