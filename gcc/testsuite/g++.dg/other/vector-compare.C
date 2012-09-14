/* { dg-do compile } */
/* { dg-options "-std=gnu++11 -Wall" } */

// Check that we can compare vector types that really are the same through
// typedefs.

typedef float v4f __attribute__((vector_size(4*sizeof(float))));

template <class T> void eat (T&&) {}

template <class T, int n>
struct Vec
{
  typedef T type __attribute__((vector_size(4*sizeof(T))));

  template <class U>
  static void fun (type const& t, U& u) { eat (t > u); }
};

long long
f (v4f *x, v4f const *y)
{
  return ((*x < *y) | (*x <= *y))[2];
}

int main ()
{
  v4f x = {0,1,2,3};
  Vec<const volatile float,4>::type f = {-1,5,2,3.1};
  auto c = (x == f) == (x >= x);
  eat (c[3]);
  Vec<const volatile float,4>::fun (f, x);
  Vec<const volatile float,4>::fun (x, f);
  Vec<const volatile float,4>::fun (f, f);
  Vec<const volatile float,4>::fun (x, x);
}
