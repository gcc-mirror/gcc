// PR c/102989
// { dg-do compile }

_BitInt(63) a;			// { dg-error "expected" }
unsigned _BitInt(31) b;		// { dg-error "expected" }
int c = 21wb;			// { dg-error "invalid suffix 'wb' on integer constant" "" { target c++98_only } }
				// { dg-error "unable to find numeric literal operator 'operator\"\"wb'" "" { target c++11 } .-1 }
long long d = 60594869054uwb;	// { dg-error "invalid suffix 'uwb' on integer constant" "" { target c++98_only } }
				// { dg-error "unable to find numeric literal operator 'operator\"\"uwb'" "" { target c++11 } .-1 }
