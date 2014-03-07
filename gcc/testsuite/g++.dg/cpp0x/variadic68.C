// { dg-do run { target c++11 } }
extern "C" void abort();

template<typename T, T... Values>
void f(T* expected_values, int n)
{
  if (sizeof...(Values) != n)
    abort ();

  T values[] = { Values... };
  for (int i = 0; i < n; ++i)
    if (values[i] != expected_values[i])
      abort();
}

int main()
{
  int test_arr1[3] = { 1, 2, 3 };
  f<int, 1, 2, 3>(test_arr1, 3);

  return 0;
}
