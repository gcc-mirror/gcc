// PR c++/89074
// { dg-do compile { target c++14 } }

constexpr bool
foo ()
{
  int a[] = { 1, 2 };
  int b[] = { 3, 4 };

  if (&a[0] == &b[0])
    return false;

  if (&a[1] == &b[0])
    return false;

  if (&a[1] == &b[1])
    return false;

  if (&a[2] == &b[1])
    return false;

  if (&a[2] == &b[0])		// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool a = foo ();
