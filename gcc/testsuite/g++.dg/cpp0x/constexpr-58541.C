// { dg-do compile { target c++11 } }

struct X {
 static constexpr const char x[] = "x";
};
const char X::x[];
