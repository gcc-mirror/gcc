// PR c++/118590
// { dg-do compile }

template <typename T>
struct A
{
  int z;
};

template <typename T, typename U>
struct B
{
  char *w;
  A<T> y;
};

template <typename T, typename U>
void
foo  (B<T, U> &x)
{
  A<T> c = x.y;
  #pragma acc enter data copyin(x.w[0 : c.z])
}

void
bar (B<int, int> &x)
{
  foo (x);
}
