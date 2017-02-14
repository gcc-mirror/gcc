// { dg-options -std=c++1z }

template <class T, T n> void f(T, int (&)[n]);
template <class T, T n> void g(int (&)[n], T);
template <class T, T n> void h(int (&)[n]);

int main()
{
  const int i = 42;
  int ar[i];
  h(ar);
  f(i, ar);
  g(ar, i);
}
