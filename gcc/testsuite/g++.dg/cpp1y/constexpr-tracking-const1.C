// PR c++/91264
// { dg-do compile { target c++14 } }

constexpr void
mod (int &r)
{
  r = 99; // { dg-error "modifying a const object" }
}

constexpr int
fn1 ()
{
  const int i = 0; // { dg-message "originally declared" }
  mod (const_cast<int &>(i)); // { dg-message "in .constexpr. expansion of " }
  return i;
}

constexpr int i1 = fn1 (); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn2 ()
{
  const int i = 5; // { dg-message "originally declared" }
  const_cast<int &>(i) = 10; // { dg-error "modifying a const object" }
  return i;
}

constexpr int i2 = fn2 (); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn3 ()
{
  const int i = 5; // { dg-message "originally declared" }
  ++const_cast<int &>(i); // { dg-error "modifying a const object" }
  return i;
}

constexpr int i3 = fn3 (); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn4 ()
{
  const int i = 5; // { dg-message "originally declared" }
  const_cast<int &>(i)--; // { dg-error "modifying a const object" }
  return i;
}

constexpr int i4 = fn4 (); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn5 ()
{
  const int i = 5; // { dg-message "originally declared" }
  const_cast<int &>(i) += 2; // { dg-error "modifying a const object" }
  return i;
}

constexpr int i5 = fn5 (); // { dg-message "in .constexpr. expansion of " }

constexpr int
fn6 ()
{
  // This is OK.
  int i = 3;
  const int *cip = &i;
  int *ip = const_cast<int *>(cip);
  *ip = 4;
  return i;
}

constexpr int i6 = fn6 ();
static_assert(i6 == 4, "");
