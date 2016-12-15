// PR c++/77637
// { dg-do compile { target c++11 } }

int [[...]] a;		// { dg-error "expected attribute before '...'" }
int [[,,...]] b;	// { dg-error "expected attribute before '...'" }
