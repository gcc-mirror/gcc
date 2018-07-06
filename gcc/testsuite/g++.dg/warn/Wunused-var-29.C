// PR c++/83690
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-variable" }

void
foo ()
{
  constexpr bool foo = true;		// { dg-bogus "set but not used" }
  static_assert (foo, "foo");
}
