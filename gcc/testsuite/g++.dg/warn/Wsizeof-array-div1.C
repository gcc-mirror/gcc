// PR c++/91741
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

int
fn1 ()
{
  int arr[10];
  return sizeof (arr) / sizeof (decltype(arr[0]));
}

template<typename T, int N>
int fn2 (T (&arr)[N])
{
  return sizeof (arr) / sizeof (T);
}

template<typename T, int N>
int fn3 (T (&arr)[N])
{
  return sizeof (arr) / sizeof (bool); // { dg-warning "expression does not compute" }
}

template<typename U, int N, typename T>
int fn4 (T (&arr)[N])
{
  return sizeof (arr) / sizeof (U); // { dg-warning "expression does not compute" }
}

void
fn ()
{
  int arr[10];
  fn2 (arr);
  fn3 (arr);
  fn4<short> (arr);
}
