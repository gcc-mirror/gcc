// C++26 P4136R2 - #line is not in line with existing implementation
// { dg-do compile }
// { dg-options "-pedantic-errors" }

#line 6
#line 0				// { dg-error "line number out of range" }
#line 8
#line 2147483647		// { dg-error "line number out of range" "" { target c++98_only } }
#line 10
#line 2147483648		// { dg-error "line number out of range" }
#line 12
#line 4294967295		// { dg-error "line number out of range" }
#line 14
#line 4294967296		// { dg-error "line number out of range" }
#line 16
#line 9223372036854775807	// { dg-error "line number out of range" }
#line 18
#line 9223372036854775808	// { dg-error "line number out of range" }
