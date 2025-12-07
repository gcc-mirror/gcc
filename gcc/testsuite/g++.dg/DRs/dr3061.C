// DR 3061 - Trailing comma in an expansion-init-list
// { dg-do compile { target c++11 } }
// { dg-options "" }

#include <initializer_list>

void
foo ()
{
  for (int x : { 1, })
    ;
  template for (int x : { 1, })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;
  for (int x : { , })			// { dg-error "expected primary-expression before ',' token" }
    ;
  template for (int x : { , })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "expected primary-expression before ',' token" "" { target *-*-* } .-1 }
}
