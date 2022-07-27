// P2242R3
// { dg-do compile { target c++14 } }

constexpr int
foo ()
{
lab:		// { dg-error "label definition in 'constexpr' function only available with" "" { target c++20_down } }
  return 1;
}

constexpr int
bar (int x)
{
  if (x)
    goto lab;	// { dg-error "'goto' in 'constexpr' function only available with" "" { target c++20_down } }
  return 1;
lab:
  return 0;
}

constexpr int
baz (int x)
{
  if (!x)
    return 1;
  static int a;	// { dg-error "'a' defined 'static' in 'constexpr' function only available with" "" { target c++20_down } }
  return ++a;	// { dg-error "uninitialized variable 'a' in 'constexpr' function" "" { target c++17_down } .-1 }
}

constexpr int
qux (int x)
{
  if (!x)
    return 1;
  thread_local int a;	// { dg-error "'a' defined 'thread_local' in 'constexpr' function only available with" "" { target c++20_down } }
  return ++a;	// { dg-error "uninitialized variable 'a' in 'constexpr' function" "" { target c++17_down } .-1 }
}

constexpr int
garply (int x)
{
  if (!x)
    return 1;
  extern thread_local int a;	// { dg-bogus "'thread_local' in 'constexpr' function only available with" "" { target c++20_down } }
  return ++a;
}

struct S { S (); ~S (); int s; };	// { dg-message "'S' is not literal because:" "" { target c++20_down } }
					// { dg-message "'S' has a non-trivial destructor" "" { target c++17_down } .-1 }
					// { dg-message "'S' does not have 'constexpr' destructor" "" { target { c++20_only } } .-2 }

constexpr int
corge (int x)
{
  if (!x)
    return 1;
  S s;			// { dg-error "variable 's' of non-literal type 'S' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;
}

#if __cpp_constexpr >= 202110L
static_assert (foo ());
static_assert (bar (0));
static_assert (baz (0));
static_assert (qux (0));
static_assert (garply (0));
static_assert (corge (0));
#endif
