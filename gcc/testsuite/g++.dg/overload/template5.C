// { dg-do compile }

template<typename T>
int low(T a, T b, T c) { return a + b + c; } // { dg-message "template" }
// { dg-message "(candidate|3 arguments, 2 provided)" "" { target *-*-* } .-1 }

template<typename T>
int high(T a, T b, T c) { return a + b + c; } // { dg-message "template" }
// { dg-message "(candidate|3 arguments, 4 provided)" "" { target *-*-* } .-1 }

void test (void)
{
  low (5, 6);			// { dg-error "no matching function" }
  high (5, 6, 7, 8);		// { dg-error "no matching function" }
}
