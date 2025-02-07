// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-variable -Wunused-but-set-variable -Wunused-parameter -Wshadow" }

int bar ();

void
baz ()
{
  for (; int _ = bar (); ++_)
    int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  for (int _ = bar ();
       int _ = bar (); )	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    int _ = 2;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  for (int _ = bar ();
       int _ = bar (); )	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ;
  for (; int _ = bar (); ++_)
    {
      int _ = 3;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
  for (int _ = bar ();
       int _ = bar (); )	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {
      int _ = 4;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
}
