// PR c++/91388
// Make sure we don't emit -Wreturn-type in functions with failed static_asserts.
// { dg-do compile { target c++17 } }

template <int N>
int
foo ()
{
  if constexpr (N <= 42)
    return N;
  else
    static_assert (false, "too high N");	// { dg-error "too high N" }
}						// { dg-bogus "no return statement in function returning non-void" }

int a = foo <0> ();
int b = foo <42> ();
int c = foo <100> ();
