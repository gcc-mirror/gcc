template <typename T>
int foo(T *x)
{
  int res;
  #pragma omp target device_type(nohost) map(from:res)
    res = x[0];
  return res;
}

int f(int *y) {
  return foo<int>(y);
}
