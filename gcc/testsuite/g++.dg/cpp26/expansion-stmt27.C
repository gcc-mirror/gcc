// PR c++/122788
// { dg-do compile { target c++14 } }
// { dg-options "-Wunused" }

void
foo ()
{
  template for ([[maybe_unused]] auto i : { 42 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;								// { dg-bogus "unused variable 'i'" "" { target *-*-* } .-1 }
  template for ([[maybe_unused]] auto j : { 1, 2, 3 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {								// { dg-bogus "unused variable 'j'" "" { target *-*-* } .-1 }
#if __cpp_if_constexpr >= 201606
      if constexpr (false)
	(void) j;
#endif
    }
}
