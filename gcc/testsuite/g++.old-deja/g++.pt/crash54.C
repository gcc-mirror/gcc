// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
int g (T);

int j = g (3);

template <class T>
inline T f (T)
{
  return 2;
}

template <class T>
struct S
{
  static const int i;
};

template <class T>
const int S<T>::i = f (3);

template <class T>
int g (T)
{
  return S<double>::i;
}

