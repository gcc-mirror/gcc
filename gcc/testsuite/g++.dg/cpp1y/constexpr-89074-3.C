// PR c++/89074
// { dg-do compile { target c++14 } }

int fn1 (void) { return 0; }
int fn2 (void) { return 1; }

constexpr bool
f1 ()
{
  char a[] = { 1, 2, 3, 4 };

  if (&a[1] == "foo")
    return false;

  if (&a[1] == &"foo"[4])
    return false;

  if (&"foo"[1] == &a[0])
    return false;

  if (&"foo"[3] == &a[4])
    return false;

  if (&a[0] == "foo")
    return false;

  // Pointer to start of one object (var) and end of another one (literal)
  if (&a[0] == &"foo"[4])	// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool
f2 ()
{
  char a[] = { 1, 2, 3, 4 };

  // Pointer to end of one object (var) and start of another one (literal)
  if (&a[4] == "foo")		// { dg-error "is not a constant expression" }
    return false;

  return true;
}

char v[] = { 1, 2, 3, 4 };

constexpr bool
f3 ()
{
  char a[] = { 1, 2, 3, 4 };

  if (&a[1] == &v[1])
    return false;

  if (&a[0] == &v[3])
    return false;

  if (&a[2] == &v[4])
    return false;

  // Pointer to start of one object (automatic var) and end of another one (non-automagic var)
  if (&a[0] == &v[4])		// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool
f4 ()
{
  char a[] = { 1, 2, 3, 4, 5 };

  // Pointer to end of one object (automatic var) and start of another one (non-automagic var)
  if (&a[5] == &v[0])		// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool
f5 ()
{
  if (fn1 != fn1)
    return false;

  if (fn1 == fn2)
    return false;

  if (&"abcde"[0] == &"edcba"[1])
    return false;

  if (&"abcde"[1] == &"edcba"[6])
    return false;

  // Pointer to start of one object (literal) and end of another one (literal)
  if (&"abcde"[0] == &"edcba"[6])	// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool
f6 ()
{
  // Pointer to start of one object (literal) and end of another one (literal)
  if (&"abcde"[6] == &"edcba"[0])	// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool
f7 ()
{
  if (&"abcde"[3] == &"fabcde"[3])
    return false;

  // These could be suffix merged, with &"abcde"[0] == &"fabcde"[1].
  if (&"abcde"[3] == &"fabcde"[4])	// { dg-error "is not a constant expression" }
    return false;

  return true;
}

constexpr bool a = f1 ();
constexpr bool b = f2 ();
constexpr bool c = f3 ();
constexpr bool d = f4 ();
constexpr bool e = f5 ();
constexpr bool f = f6 ();
constexpr bool g = f7 ();
