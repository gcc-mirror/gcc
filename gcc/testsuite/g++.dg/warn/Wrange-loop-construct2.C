// PR c++/94695
// { dg-do compile { target c++11 } }
// { dg-options "-Wrange-loop-construct" }

struct Foo { };
struct Bar {
  char arr[100];
  Bar(Foo);
  Bar(int);
  operator int();
};

template<typename T>
struct It {
  T operator*();
  It operator++();
  bool operator!=(const It);
};

template<typename T>
struct Cont {
  using I = It<T>;
  I begin();
  I end();
};

void
fn1 ()
{
  int arr[10];
  Cont<int> cont_int;

  for (const double &x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const double x : arr) { (void) x; }
  for (const int &x : arr) { (void) x; }
  for (double &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (double x : arr) { (void) x; }

  for (const int &&x : cont_int) { (void) x; }
  for (const int &x : cont_int) { (void) x; }
  for (const int x : cont_int) { (void) x; }
  for (int&& x : cont_int) { (void) x; }
  for (int x : cont_int) { (void) x; }

  for (const double &&x : cont_int) { (void) x; }
  for (const double &x : cont_int) { (void) x; }
  for (const double x : cont_int) { (void) x; }

  for (double &&x : cont_int) { (void) x; }
  for (double x : cont_int) { (void) x; }

  for (const Bar &&x : cont_int) { (void) x; }
  for (const Bar x : cont_int) { (void) x; }
}

void
fn2 ()
{
  Cont<int &> cont_int_ref;

  for (const int &x : cont_int_ref) { (void) x; }
  for (const int x : cont_int_ref) { (void) x; }
  for (int &x : cont_int_ref) { (void) x; }
  for (int x : cont_int_ref) { (void) x; }

  for (const double &&x : cont_int_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const double &x : cont_int_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const double x : cont_int_ref) { (void) x; }
  for (double &&x : cont_int_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (double x : cont_int_ref) { (void) x; }

  for (const Bar &&x : cont_int_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar &x : cont_int_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar x : cont_int_ref) { (void) x; }
  for (Bar &&x : cont_int_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (Bar x : cont_int_ref) { (void) x; }
}

void
fn3 ()
{
  Cont<Bar> cont_bar;

  for (const Bar &&x : cont_bar) { (void) x; }
  for (const Bar &x : cont_bar) { (void) x; }
  for (const Bar x : cont_bar) { (void) x; }
  for (Bar &&x : cont_bar) { (void) x; }
  for (Bar x : cont_bar) { (void) x; }

  for (const int &&x : cont_bar) { (void) x; }
  for (const int &x : cont_bar) { (void) x; }
  for (const int x : cont_bar) { (void) x; }
  for (int &&x : cont_bar) { (void) x; }
  for (int x : cont_bar) { (void) x; }
}

void
fn4 ()
{
  Cont<Bar &> cont_bar_ref;

  for (const Bar &x : cont_bar_ref) { (void) x; }
  for (Bar &x : cont_bar_ref) { (void) x; }
  for (Bar x : cont_bar_ref) { (void) x; }

  for (const int &&x : cont_bar_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const int &x : cont_bar_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const int x : cont_bar_ref) { (void) x; }
  for (int &&x : cont_bar_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (int x : cont_bar_ref) { (void) x; }
}

void
fn5 ()
{
  Cont<Foo> cont_foo;

  for (const Bar &&x : cont_foo) { (void) x; }
  for (const Bar &x : cont_foo) { (void) x; }
  for (const Bar x : cont_foo) { (void) x; }
  for (Bar &&x : cont_foo) { (void) x; }
  for (Bar x : cont_foo) { (void) x; }
}

void
fn6 ()
{
  Cont<Foo &> cont_foo_ref;

  for (const Bar &&x : cont_foo_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar &x : cont_foo_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar x : cont_foo_ref) { (void) x; }
  for (Bar &&x : cont_foo_ref) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (Bar x : cont_foo_ref) { (void) x; }
}

void
fn7 ()
{
  double arr[2];

  for (const double &x : arr) { (void) x; }
  for (const double x : arr) { (void) x; }
  for (double &x : arr) { (void) x; }
  for (double x : arr) { (void) x; }

  for (const int &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const int &x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const int x : arr) { (void) x; }
  for (int &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (int x : arr) { (void) x; }

  for (const Bar &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar &x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar x : arr) { (void) x; }
  for (Bar &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (Bar x : arr) { (void) x; }
}

void
fn8 ()
{
  Foo arr[2];

  for (const Foo &x : arr) { (void) x; }
  for (const Foo x : arr) { (void) x; }
  for (Foo &x : arr) { (void) x; }
  for (Foo x : arr) { (void) x; }

  for (const Bar &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar &x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const Bar x : arr) { (void) x; }
  for (Bar &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (Bar x : arr) { (void) x; }
}

void
fn9 ()
{
  Bar arr[2] = { 1, 2 };

  for (const Bar &x : arr) { (void) x; }
  for (Bar &x : arr) { (void) x; }
  for (Bar x : arr) { (void) x; }

  for (const int &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const int &x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (const int x : arr) { (void) x; }
  for (int &&x : arr) { (void) x; } // { dg-warning "binds to a temporary constructed from type" }
  for (int x : arr) { (void) x; }
}

template<typename T>
void
fn10 ()
{
  Cont<Bar> cont_bar;

  for (const Bar &x : cont_bar) { (void) x; }

  Cont<T> cont_dep;
  for (const T &x : cont_dep) { (void) x; }
}
template void fn10<Bar>();

struct S {
  void fn()
  {
    Cont<Bar> cont_bar;
    for (const Bar &x : cont_bar) { (void) x; }
  }
};
