// Test that very different operators still cause ambiguity with reversed.

struct X { operator int(); };
bool operator==(X, int);    // #1 { dg-message "reversed" "" { target c++2a } }
struct Y { operator int(); };
bool operator==(Y, int);    // #2 { dg-message "reversed" "" { target c++2a } }

X x; Y y;
bool b1 = x == y;		// { dg-error "ambiguous" "" { target c++2a } }
bool b2 = y == x;		// { dg-error "ambiguous" "" { target c++2a } }
