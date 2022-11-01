// P1774R8 - Portable assumptions
// { dg-do compile { target c++11 } }

[[__assume__ (true)]] void f1 ();		// { dg-error "'assume' attribute ignored" }
typedef int intx [[__assume__ (true)]];		// { dg-error "'assume' attribute ignored" }
[[__assume__ (true)]];				// { dg-warning "attribute ignored" }
[[gnu::assume (true)]] void f1a ();		// { dg-error "'assume' attribute ignored" }
typedef int inty [[gnu::__assume__ (true)]];	// { dg-error "'assume' attribute ignored" }
[[__gnu__::assume (true)]];			// { dg-warning "attribute ignored" }
__attribute__((assume (true))) void f1b ();	// { dg-error "'assume' attribute ignored" }
typedef int intz __attribute__((assume (true)));// { dg-error "'assume' attribute ignored" }

void
foo ()
{
  int i;
  [[__assume__]];			// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-1 }
  [[__assume__ ()]];			// { dg-error "parentheses must be omitted if 'assume' attribute argument list is empty" }
					// { dg-error "wrong number of arguments specified for 'assume' attribute" "" { target *-*-* } .-1 }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-2 }
  [[__assume__ (true, true)]];		// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 2" "" { target *-*-* } .-1 }
  [[__assume__ (true)]] i = 1;		// { dg-warning "'assume' attribute not followed by ';'" }
  [[__assume__ (throw 1)]];		// { dg-error "expected primary-expression before 'throw'" }
  [[__assume__ (i = 1)]];		// { dg-error "expected '\\\)' before '=' token" }
  [[gnu::assume]];			// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-1 }
  [[gnu::assume ()]];			// { dg-error "parentheses must be omitted if 'assume' attribute argument list is empty" }
					// { dg-error "wrong number of arguments specified for 'assume' attribute" "" { target *-*-* } .-1 }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-2 }
  [[gnu::assume (true, true)]];		// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 2" "" { target *-*-* } .-1 }
  [[gnu::assume (true)]] i = 1;		// { dg-warning "'assume' attribute not followed by ';'" }
  [[gnu::assume (throw 1)]];		// { dg-error "expected primary-expression before 'throw'" }
  [[gnu::assume (i = 1)]];		// { dg-error "expected '\\\)' before '=' token" }
  __attribute__((assume));		// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-1 }
  __attribute__((assume ()));		// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 0" "" { target *-*-* } .-1 }
  __attribute__((assume (true, true)));	// { dg-error "wrong number of arguments specified for 'assume' attribute" }
					// { dg-message "expected 1, found 2" "" { target *-*-* } .-1 }
  __attribute__((assume (true))) i = 1;	// { dg-warning "'assume' attribute not followed by ';'" }
  __attribute__((assume (throw 1)));	// { dg-error "expected primary-expression before 'throw'" }
  __attribute__((assume (i = 1)));	// { dg-error "expected '\\\)' before '=' token" }
}

constexpr int
f2 (int x)
{
#if __cpp_constexpr >= 201304L
  [[__assume__ (x == 42)]];		// { dg-error "failed 'assume' attribute assumption" "" { target c++14 } }
#endif
  return x;
}

constexpr int
f2a (int x)
{
#if __cpp_constexpr >= 201304L
  [[gnu::__assume__ (x == 42)]];	// { dg-error "failed 'assume' attribute assumption" "" { target c++14 } }
#endif
  return x;
}

constexpr int
f2b (int x)
{
#if __cpp_constexpr >= 201304L
  __attribute__((__assume__ (x == 42)));// { dg-error "failed 'assume' attribute assumption" "" { target c++14 } }
#endif
  return x;
}

constexpr int a = f2 (44);
constexpr int aa = f2a (44);
constexpr int ab = f2b (44);

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
  [[__assume__ (f3 (42) == 42)]];
#endif
  return x;
}

constexpr int
f4a (int x)
{
#if __cpp_constexpr >= 201304L
  [[gnu::assume (f3 (42) == 42)]];
#endif
  return x;
}

constexpr int
f4b (int x)
{
#if __cpp_constexpr >= 201304L
  __attribute__((assume (f3 (42) == 42)));
#endif
  return x;
}

static_assert (f4 (42) == 42, "");
static_assert (f4a (42) == 42, "");
static_assert (f4b (42) == 42, "");

struct S {};

int
f5 ()
{
  S s;
  [[gnu::assume (s)]];			// { dg-error "could not convert 's' from 'S' to 'bool'" }
  return 0;
}

template <typename T>
int
f6 ()
{
  T t;
  __attribute__((assume (t)));		// { dg-error "could not convert 't' from 'S' to 'bool'" }
  return 0;
}

int z = f6 <S> ();
