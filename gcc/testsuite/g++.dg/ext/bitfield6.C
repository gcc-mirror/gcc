// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  char a [[gnu::packed]] = 1;		// { dg-warning "attribute ignored for field of type" }
  char b [[gnu::packed]] : 8;
  char c [[gnu::packed]] : 8 = 2;	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
};
template <typename U>
struct T {
  U d [[gnu::packed]] = 1;		// { dg-warning "attribute ignored for field of type" }
  U e [[gnu::packed]] : 8;
  U f [[gnu::packed]] : 8 = 2;		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
};
T<char> t;
