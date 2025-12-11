// { dg-do compile }

typedef int& R;

template <typename T>
void foo (T x)
{
  foo (R (x));
}
