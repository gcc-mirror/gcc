// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "" }

int bar ();

void
baz ()
{
  for (int _ = bar ();
       int _ = bar (); ++_)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ;				// { dg-error "reference to '_' is ambiguous" "" { target *-*-* } .-1 }
  for (; int _ = bar (); ++_)
    {
      int _ = 3;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  for (int _ = bar ();
       int _ = bar (); )	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {
      int _ = 4;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
}
