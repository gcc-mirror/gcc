// PR c++/89336
// { dg-do compile { target c++14 } }

constexpr int
foo ()
{
  union U { int a; long b; };
  union V { union U u; short v; };
  V w {};
  w.u.a = w.v = w.u.b = 5L;		// { dg-error "change of the active member of a union from" "" { target c++17_down } }
  return w.u.a;
}

static_assert (foo () == 5, "");	// { dg-error "non-constant condition for static assertion" "" { target c++17_down } }
					// { dg-message "expansion of" "" { target c++17_down } .-1 }

constexpr int
bar ()
{
  union U { int a[5]; long b; };
  union V { union U u; short v; };
  V w {};
  w.v = 5;
  w.u.a[3] = w.u.a[1] = w.v;		// { dg-error "change of the active member of a union from" "" { target c++17_down } }
  return w.u.a[1] + w.u.a[3];
}

static_assert (bar () == 10, "");	// { dg-error "non-constant condition for static assertion" "" { target c++17_down } }
					// { dg-message "expansion of" "" { target c++17_down } .-1 }

struct Z { int x, y; };

constexpr Z
baz ()
{
  union W { Z a; long long w; };
  W w {};
  w.a = { 5, 0 };
  w.a = { (int) (w.w = 17LL + w.a.x), 2 };	// { dg-error "change of the active member of a union from" "" { target c++17_down } }
  return w.a;
}

static_assert (baz ().x == 22, "");	// { dg-error "non-constant condition for static assertion" "" { target c++17_down } }
					// { dg-message "expansion of" "" { target c++17_down } .-1 }
static_assert (baz ().y == 2, "");	// { dg-error "non-constant condition for static assertion" "" { target c++17_down } }
					// { dg-message "expansion of" "" { target c++17_down } .-1 }
