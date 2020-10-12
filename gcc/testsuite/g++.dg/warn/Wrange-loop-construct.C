// PR c++/94695
// { dg-do compile { target c++11 } }
// { dg-options "-Wrange-loop-construct" }

#include <initializer_list>

struct Small {
  char arr[64];
};

struct Big_aggr {
  char arr[65];
};

struct Big_triv_copy {
  char arr[65];
  Big_triv_copy() { }
};

struct Big {
  char arr[65];
  Big () = default;
  Big(const Big&);
};

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

#define TEST					    \
  void fn_macro()				    \
  {						    \
    Cont<Bar &> cont_bar_ref;			    \
    for (const Bar x : cont_bar_ref) { (void) x; }  \
  }

TEST

Cont<Bar &>& foo ();
Cont<Bar &> foo2 ();

void
fn1 ()
{
  for (const auto x : foo () ) { (void) x; } // { dg-warning "creates a copy" }
  for (const auto x : foo2 () ) { (void) x; } // { dg-warning "creates a copy" }

  Small s{};
  Small sa[5] = { };
  for (const auto x : sa) { (void) x; }
  for (const auto x : { s, s, s }) { (void) x; }

  Big_aggr b{};
  Big_aggr ba[5] = { };
  for (const auto x : ba) { (void) x; } // { dg-warning "creates a copy" }
  for (const auto x : { b, b, b }) { (void) x; } // { dg-warning "creates a copy" }

  Big_triv_copy bt{};
  Big_triv_copy bta[5];
  for (const auto x : bta) { (void) x; } // { dg-warning "creates a copy" }
  for (const auto x : { bt, bt, bt }) { (void) x; } // { dg-warning "creates a copy" }

  Big b2;
  Big ba2[5];
  for (const auto x : ba2) { (void) x; } // { dg-warning "creates a copy" }
  for (const auto x : { b2, b2, b2 }) { (void) x; } // { dg-warning "creates a copy" }
}

void
fn2 ()
{
  Cont<int> cont_int;
  for (const auto x : cont_int) { (void) x; }
  for (const int x : cont_int) { (void) x; }
  for (int x : cont_int) { (void) x; }
  for (const auto &x : cont_int) { (void) x; }
  for (double x : cont_int) { (void) x; }
  for (const double x : cont_int) { (void) x; }
  for (const Bar x : cont_int) { (void) x; }
  for (Bar x : cont_int) { (void) x; }
}

void
fn3 ()
{
  Cont<int &> cont_int_ref;
  for (const int x : cont_int_ref) { (void) x; }
  for (int x : cont_int_ref) { (void) x; }
  for (const double x : cont_int_ref) { (void) x; }
  for (double x : cont_int_ref) { (void) x; }
  for (const Bar x : cont_int_ref) { (void) x; }
  for (Bar x : cont_int_ref) { (void) x; }
}

void
fn4 ()
{
  Cont<Bar> cont_bar;
  for (const Bar x : cont_bar) { (void) x; }
  for (Bar x : cont_bar) { (void) x; }
  for (const int x : cont_bar) { (void) x; }
  for (int x : cont_bar) { (void) x; }
}

void
fn5 ()
{
  Cont<Bar&> cont_bar_ref;
  for (const Bar x : cont_bar_ref) { (void) x; } // { dg-warning "creates a copy" }
  for (Bar x : cont_bar_ref) { (void) x; }
  for (const int x : cont_bar_ref) { (void) x; }
  for (int x : cont_bar_ref) { (void) x; }
}

void
fn6 ()
{
  Cont<Foo> cont_foo;
  for (const Bar x : cont_foo) { (void) x; }
  for (Bar x : cont_foo) { (void) x; }
}

void
fn7 ()
{
  Cont<Foo &> cont_foo_ref;
  for (const Bar x : cont_foo_ref) { (void) x; }
  for (Bar x : cont_foo_ref) { (void) x; }
}

void
fn8 ()
{
  double arr[2];
  for (const double x : arr) { (void) x; }
  for (double x : arr) { (void) x; }
  for (const int x : arr) { (void) x; }
  for (int x : arr) { (void) x; }
  for (const Bar x : arr) { (void) x; }
  for (Bar x : arr) { (void) x; }
}

void
fn9 ()
{
  Foo foo[2];
  for (const Foo x : foo) { (void) x; }
  for (Foo x : foo) { (void) x; }
  for (const Bar x : foo) { (void) x; }
  for (Bar x : foo) { (void) x; }
}

void
fn10 ()
{
  Bar bar[2] = { 1, 2 };
  for (const Bar x : bar) { (void) x; } // { dg-warning "creates a copy" }
  for (Bar x : bar) { (void) x; }
  for (const int x : bar) { (void) x; }
  for (int x : bar) { (void) x; }
}

template<typename T>
void
fn11 ()
{
  Cont<Bar> cont_bar;
  for (const Bar x : cont_bar) { (void) x; }

  Cont<Bar&> cont_bar_ref;
  for (const Bar x : cont_bar_ref) { (void) x; } // { dg-warning "creates a copy" }

  Cont<T> cont_dep;
  for (const T x : cont_dep) { (void) x; }
}

template<typename T>
void
fn12 ()
{
  for (const auto x : { T{} }) { (void) x; } // { dg-warning "creates a copy" }
}

void
invoke ()
{
  fn11<int> ();
  fn12<Big> ();
}
