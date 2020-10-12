/* { dg-do run } */

typedef int T;
static const T a[2][3] __attribute__((aligned(2*sizeof(T)))) = { { 1, 2, 3 }, { 4, 5, 6 } };
typedef T v2 __attribute__((vector_size(2*sizeof(T))));

int
main()
{
  const T *p = &a[0][2];
  v2 x = *(const v2 *)p;
  T z = x[1];
  if (z != 4)
    __builtin_abort ();
  return 0;
}
