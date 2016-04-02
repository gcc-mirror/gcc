// { dg-do compile { target c++11 } }

constexpr char c1 = "hi"[1];
constexpr char c2 = "hi"[2];
constexpr char c3 = "hi"[3];	// { dg-error "array subscript" }
