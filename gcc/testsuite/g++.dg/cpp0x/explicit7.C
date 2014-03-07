// [over.match.conv]: For direct-initialization, those explicit conversion
// functions that are not hidden within S and yield type T or a type that
// can be converted to type T with a qualification conversion (4.4) are
// also candidate functions.

// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { };
struct B: A { };
struct C {
  explicit operator B*();	// { dg-message "explicit" }
  explicit operator B&();	// { dg-message "explicit" }
};

C c;
A* ap (c);			// { dg-error "" }
A& ar (c);			// { dg-error "" }
