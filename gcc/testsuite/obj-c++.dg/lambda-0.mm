// Contributed by Iain Sandoe <iain@codesourcery.com>, December 2014.  */
// { dg-do compile }
// { dg-options "-std=c++11" }


template<class Function>
Function thing(Function fn, int a)
{
  fn(a);
  return fn;
}

int
test (int *arr, unsigned n)
{
  int total = 0;
  for (unsigned i=0; i<n; i++) {
    int a = arr[i];
    thing ([&total] (int a) { total += a; }, a);
  }
  return total;
}
