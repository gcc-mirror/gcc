/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */

#define SIZE 100
#define CHECK_VALUE 5

template <class T>
int func (T start, T end)
{
  int Array[SIZE];
  _Cilk_for (T ii = 0; ii < end; ii++)
    Array[ii] = CHECK_VALUE;

  for (T ii = 0; ii < end; ii++)
    if (Array[ii] != CHECK_VALUE)
      __builtin_abort ();

  return 0;
}

int main (void)
{
  return func <int> (0, 100) + func <long> (0, 100);
}
