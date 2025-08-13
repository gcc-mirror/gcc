// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++11 } }
// { dg-options "" }

int a;
long b;

void
foo ()
{
  template for (auto g : { &a, &b, 2L, &a })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {						// { dg-message "required from here" "" { target *-*-* } .-1 }
      decltype (*g) h = *g;			// { dg-error "invalid type argument of unary" }
    }
}

// { dg-message "In instantiation of 'template for' iteration 3:" "" { target *-*-* } 0 }
