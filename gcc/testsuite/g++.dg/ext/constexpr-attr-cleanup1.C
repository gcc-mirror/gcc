// { dg-do compile { target c++2a } }

constexpr void
cleanup (int *x)
{
  if (x)
    asm ("");		// { dg-error "inline assembly is not a constant expression" }
}			// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }

constexpr void
cleanup2 (int *x)
{
}

constexpr bool
foo ()
{
  int a __attribute__((cleanup (cleanup))) = 1;	// { dg-message "in 'constexpr' expansion of" }
  return true;
}

constexpr bool
bar ()
{
  int a __attribute__((cleanup (cleanup2))) = 1;
  return true;
}

constexpr auto x = foo ();	// { dg-message "in 'constexpr' expansion of" }
constexpr auto y = bar ();
