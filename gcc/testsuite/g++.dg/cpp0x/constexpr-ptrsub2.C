// { dg-do compile { target c++11 } }

char c;
constexpr char p2 = *(&c - 1);	// { dg-error "-1" }
