// PR c++/116731
// { dg-do compile { target c++11 } }
// { dg-options "-Wrange-loop-construct" }

void
f0 ()
{
  struct S {
    char a[64];
    S& operator=(const S&) { return *this; };
  };

  S arr[8];
  for (const auto r : arr)
    (void) r;
}

void
f1 ()
{
  struct S {
    char a[65];
    S& operator=(const S&) { return *this; };
  };

  S arr[8];
  for (const auto r : arr) // { dg-warning "creates a copy" }
    (void) r;
}

void
f2 ()
{
  struct S {
    char a[64];
    S& operator=(const S&) { return *this; };
    ~S() { }
  };

  S arr[8];
  for (const auto r : arr) // { dg-warning "creates a copy" }
    (void) r;
}

void
f3 ()
{
  struct S {
    char a[65];
    S& operator=(const S&) { return *this; };
    ~S() { }
  };

  S arr[8];
  for (const auto r : arr) // { dg-warning "creates a copy" }
    (void) r;
}
