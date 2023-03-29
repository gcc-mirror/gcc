// P1668R1: Permit unevaluated inline asm in constexpr functions
// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wno-pedantic" }

constexpr int
foo ()
{
 constexpr int i = ({ asm(""); 42; }); // { dg-error "inline assembly is not a constant expression" }
 return i;
}

constexpr int i = foo ();	// { dg-error "foo" }
