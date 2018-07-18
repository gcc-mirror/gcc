// { dg-do compile }

template<typename T>
int low(T a, T b, T c) { return a + b + c; } // { dg-message "template" }

template<typename T>
int high(T a, T b, T c) { return a + b + c; } // { dg-message "template" }

void test (void)
{
  low (5, 6);			// { dg-error "no matching function" }
  // { dg-message "(candidate|3 arguments, 2 provided)" "" { target *-*-* } .-1 }
  high (5, 6, 7, 8);		// { dg-error "no matching function" }
  // { dg-message "(candidate|3 arguments, 4 provided)" "" { target *-*-* } .-1 }
}
