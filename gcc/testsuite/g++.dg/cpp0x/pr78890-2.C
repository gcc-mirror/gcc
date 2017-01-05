// PR c++/78890
// { dg-do compile { target c++11 } }

template <typename T>
int
foo ()
{
  union {
    int a;
    int &b = a;			// { dg-error "may not have reference type" }
  };
  a = 1;
  auto c = b + 1;
  return c;
}

template <typename T>
T
bar ()
{
  union {
    T a;
    T &b = a;			// { dg-error "may not have reference type" }
  };
  a = 1;
  auto c = b + 1;
  return c;
}

template <typename T, typename U>
T baz()
{
  union {
    T a;
    U b = a;			// { dg-error "may not have reference type" }
  };
  a = 1;
  auto c = b + 1;
  return c;
}

int a = foo<int> ();
int b = bar<int> ();
int c = baz<int, int &> ();
