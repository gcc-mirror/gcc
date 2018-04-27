// { dg-require-effective-target c++11 }

void test_1 ()
{
  int arr[] = {1, 2, 3, 4, 5};
  for (const auto v: arr) {
    _forbegin; // { dg-bogus "suggested alternative" }
    // { dg-error "'_forbegin' was not declared in this scope" "" { target *-*-*} .-1 }
  }
}

int test_2 ()
{
  int arr[] = {1, 2, 3, 4, 5};
  int sum = 0;
  for (const auto v: arr) {
    sum += v;
    // TODO: should we issue an error for the following assignment?
    __for_begin = __for_end;
  }
  return sum;
}
