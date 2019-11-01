// { dg-do compile }

consteval int bar (void) { return 0; }	// { dg-error "'consteval' does not name a type" "" { target c++17_down } }
