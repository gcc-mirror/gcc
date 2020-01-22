// PR c++/93228
// { dg-do compile { target c++14 } }

template <typename T>
struct [[deprecated("foo")]] bar {};	// { dg-message "declared here" }
struct [[deprecated("baz")]] qux {}; 	// { dg-message "declared here" }

void
quux ()
{
  bar<int> b;	// { dg-warning "is deprecated: foo" }
  qux c;	// { dg-warning "is deprecated: baz" }
}
