// { dg-do compile }

void
nonconst(int n)
{
  new (long[n][n]); // { dg-error "variable length|array size|not a constant|runtime bound" }
  new long[n][n]; // { dg-error "variable length|array size|not a constant" }
}

template <typename T>
void *
callnew(int n)
{
  return new long[n][T::n];
}

template <typename T>
void *
callnew_fail_1(int n)
{
  return new long[n][T::n]; // { dg-error "variable length|array size|usable in a constant" }
}

template <typename T>
void *
callnew_fail_2()
{
  return new long[T::n]; // { dg-error "size in array new" }
}

template <typename T>
void *
callnew_fail_3()
{
  return new T[2][T::n]; // { dg-error "size of array has non-integral type" }
}

struct T1 {
  static int n;
};

struct T2 {
  static const double n = 2; // { dg-error "23:'constexpr' needed" "" { target c++11 } }
  // { dg-error "23:ISO C\\+\\+ forbids" "" { target c++98_only } 43 }
};

struct T3 {
  static const int n = 2;
};

struct T4 {
  enum { n = 3 };
};

void
test_callnew(int n)
{
  new long[0.2]; // { dg-error "integral or enumeration type" }
  callnew_fail_1<T1>(n);
  callnew_fail_2<T2>();
  callnew_fail_3<T2>();
  callnew<T3>(n);
  callnew<T4>(n);
}
