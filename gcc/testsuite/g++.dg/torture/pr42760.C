// PR middle-end/42760
// { dg-do compile }

template <typename T>
struct A
{
  static T b (T it) { return it; }
};

template <typename T, typename U>
static U
baz (T x, T y, U z)
{
  for (long n = y - x; n > 0; --n)
    {
      *z = *x;
      ++z;
    }
};

template <typename T, typename U>
U
bar (T x, T y, U z)
{
  baz (A <T>::b (x), A <T>::b (y), A <U>::b (z));
}

struct C
{
  __complex__ float v;
};

template <class T>
struct B
{
  B (T y[]) { bar (y, y + 1, x); }
  operator T *() { return x; }
  T x[1];
};

B <C>
foo ()
{
  C y[1];
  return B <C> (y);
};
