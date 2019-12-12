// PR c++/89481
// { dg-do compile { target c++14 } }

constexpr int
foo ()
{
  union U { long long a; int b[2]; } u { 5LL };
  u.b[1] = 4;		// { dg-error "change of the active member of a union from" "" { target c++17_down } }
  return u.b[0];
}

constexpr int
bar ()
{
  union U { long long a; int b[2]; } u { 5LL };
  u.b[1] = 4;		// { dg-error "change of the active member of a union from" "" { target c++17_down } }
  return u.b[1];
}

static_assert (foo () == 0, "");	// { dg-error "non-constant condition for static assertion" }
					// { dg-message "in 'constexpr' expansion of" "" { target *-*-* } .-1 }
					// { dg-error "accessing uninitialized array element" "" { target c++2a } .-2 }
static_assert (bar () == 4, "");	// { dg-error "non-constant condition for static assertion" "" { target c++17_down } }
					// { dg-message "in 'constexpr' expansion of" "" { target c++17_down } .-1 }
