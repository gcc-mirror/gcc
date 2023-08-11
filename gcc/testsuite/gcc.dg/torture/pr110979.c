/* { dg-do run } */
/* { dg-additional-options "--param vect-partial-vector-usage=2" } */

#define FLT double
#define N 20

__attribute__((noipa))
FLT
foo3 (FLT *a)
{
  FLT sum = -0.0;
  for (int i = 0; i != N; i++)
    sum += a[i];
  return sum;
}

int main()
{
  FLT a[N];
  for (int i = 0; i != N; i++)
    a[i] = -0.0;
  if (!__builtin_signbit(foo3(a)))
    __builtin_abort();
  return 0;
}
