// P1774R8 - Portable assumptions
// { dg-do compile { target c++11 } }

[[assume (true)]] void f1 ();		// { dg-error "'assume' attribute ignored" }
typedef int intx [[assume (true)]];	// { dg-error "'assume' attribute ignored" }
[[assume (true)]];			// { dg-warning "attribute ignored" }

void
foo ()
{
  int i;
  [[assume]];				// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-1 }
  [[assume ()]];			// { dg-error "parentheses must be omitted if 'assume' attribute argument list is empty" }
					// { dg-error "wrong number of arguments specified for 'assume' attribute" "" { target *-*-* } .-1 }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-2 }
  [[assume (true, true)]];		// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 2" "" { target *-*-* } .-1 }
  [[assume (true)]] i = 1;		// { dg-warning "'assume' attribute not followed by ';'" }
  [[assume (throw 1)]];			// { dg-error "expected primary-expression before 'throw'" }
  [[assume (i = 1)]];			// { dg-error "expected '\\\)' before '=' token" }
}

constexpr int
f2 (int x)
{
#if __cpp_constexpr >= 201304L
  [[assume (x == 42)]];			// { dg-error "failed 'assume' attribute assumption" "" { target c++14 } }
#endif					// { dg-message "the comparison reduces to '\\\(44 == 42\\\)'" "" { target c++14 } .-1 }
  return x;
}

constexpr int a = f2 (44);

int
f3 (int x)
{
  __asm ("" : "+r" (x));
  return x;
}

constexpr int
f4 (int x)
{
#if __cpp_constexpr >= 201304L
  [[assume (f3 (42) == 42)]];
#endif
  return x;
}

static_assert (f4 (42) == 42, "");

struct S {};

int
f5 ()
{
  S s;
  [[assume (s)]];			// { dg-error "could not convert 's' from 'S' to 'bool'" }
  return 0;
}

template <typename T>
int
f6 ()
{
  T t;
  [[assume (t)]];			// { dg-error "could not convert 't' from 'S' to 'bool'" }
  return 0;
}

int z = f6 <S> ();

constexpr int
f7 (int x, int y, int z, int w)
{
#if __cpp_constexpr >= 201304L
  [[assume (x == 42 && y == 43 && z == 44 && w == 45)]];	// { dg-error "failed 'assume' attribute assumption" "" { target c++14 } }
#endif					// { dg-message "the comparison reduces to '\\\(45 == 44\\\)'" "" { target c++14 } .-1 }
  return x;
}

constexpr int w = f7 (42, 43, 45, 44);
