// { dg-do assemble  }

template <class T>
struct S {};

template <class T>
inline void g(T t)
{
 here:
  S<T> st;
  goto here;
}

template <class T>
void f(T t)
{
 here:
  g(t);
  goto here;
}

void h()
{
  f(3);
}
