// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo ()
{
  extern int _;
  extern int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  ++_;			// { dg-error "reference to '_' is ambiguous" }
}
