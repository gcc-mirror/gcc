// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

struct S
{
  char a:4;			// { dg-warning "alignas' on bit-field" "" { target *-*-* } .+1 }
  char b:8 alignas(int);	// { dg-warning "ISO C\\+\\+ allows bit-field attributes only before the ':' token" }
  char c:8 [[gnu::aligned(8)]];	// { dg-warning "ISO C\\+\\+ allows bit-field attributes only before the ':' token" }
				// { dg-error "two consecutive '\\\[' shall only introduce an attribute before '\\\[' token" "" { target *-*-* } .-1 }
};
