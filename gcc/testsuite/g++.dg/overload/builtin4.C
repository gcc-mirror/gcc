// DR 1512
// PR c++/87699
// { dg-do compile { target c++11 } }

using nullptr_t = decltype(nullptr);

template<typename T>
struct S { operator T(); };

void
fn ()
{
  S<nullptr_t> s;
  // Make sure we create a builtin operator overload candidate for == and !=.
  if (s == s) { }
  if (s != s) { }

  // But not for these.
  if (s > s) { }    // { dg-error "no match for" }
  if (s < s) { }    // { dg-error "no match for" }
  if (s <= s) { }   // { dg-error "no match for" }
  if (s >= s) { }   // { dg-error "no match for" }

  S<int *> r;
  if (s == r) { }   // { dg-error "no match for" }
  if (s != r) { }   // { dg-error "no match for" }
  if (s > r) { }    // { dg-error "no match for" }
  if (s < r) { }    // { dg-error "no match for" }
  if (s >= r) { }   // { dg-error "no match for" }
  if (s <= r) { }   // { dg-error "no match for" }
}
