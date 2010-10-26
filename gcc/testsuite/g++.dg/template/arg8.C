// Test for a message indicating what template we're trying to convert
// arguments for.  We can't actually test for it directly because it
// doesn't have an associated line number, but we can test for the
// "instantiated from here" message that follows.

template <int I>
struct A { };

int i;
A<i> a;				// { dg-message "instantiated from here" }
// { dg-error "not a valid template argument" "" { target *-*-* } 10 }
// { dg-error "invalid type in declaration" "" { target *-*-* } 10 }
