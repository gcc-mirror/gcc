// P2647R1 - Permitting static constexpr variables in constexpr functions
// { dg-do compile { target c++14 } }

constexpr int
f1 (int x)
{
  if (x)
    throw 1;
  return 0;
}

constexpr int
f2 ()
{
  static const int a = f1 (1);		// { dg-error "'a' defined 'static' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;
}

constexpr int
f3 ()
{
  static const int a = 5;		// { dg-error "'a' defined 'static' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;
}

constexpr int
f4 ()
{					// { dg-message "is not usable as a 'constexpr' function because:" "" { target c++23 } .-1 }
  static const int a = f1 (1);		// { dg-error "'a' defined 'static' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;				// { dg-error "'a' defined 'static' in 'constexpr' context" "" { target c++23 } .-1 }
}

constexpr int a4 = f4 ();		// { dg-error "called in a constant expression" }

constexpr int
f5 ()
{
  static const int a = f1 (0);		// { dg-error "'a' defined 'static' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;
}

constexpr int
f6 ()
{
  static const int a = f1 (0);		// { dg-error "'a' defined 'static' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;
}

constexpr int a6 = f6 ();		// { dg-error "called in a constant expression" "" { target c++20_down } }
