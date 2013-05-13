// PR c++/56642

template <class T> struct A;

template <class T>
A<T> f(T*) { return A<T>(); }

template <class T>
struct A
{
  friend A f<T>(T*);
};

int main()
{
  int *p = 0;
  f(p);
}
