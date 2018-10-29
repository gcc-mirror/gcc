// PR c++/87594
// { dg-do compile { target c++14 } }

constexpr bool always_false() { return false; }
int f() { return 1; }

constexpr int
fn1()
{
  struct empty_range {
    constexpr int* begin() { return 0; }
    constexpr int* end() { return 0; }
  } e;
  for (auto x : e)
    f();
  return 0;
}

constexpr int
fn2 ()
{
  int a[] = { 1, 2, 3 };
  for (auto x : a)
    f(); // { dg-error "call to non-.constexpr. function" }
  return 0;
}

constexpr int
fn3 ()
{
  __extension__ int a[] = { };
  for (auto x : a)
    f();
  return 0;
}


void
bar ()
{
  constexpr int i1 = fn1 ();
  constexpr int i2 = fn2 (); // { dg-message "in .constexpr. expansion of " }
  constexpr int i3 = fn3 ();
}
