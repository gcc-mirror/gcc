// { dg-do link  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
int f(T);

template <class T>
struct S {
  template <class U>
  friend int f(U) { return 0; }
};

int k = f(2);

template <class T>
int g(T);

int h = g(7);

template <class T>
int g(T) {
  S<T> si;
  return 0;
}

int main()
{
}

