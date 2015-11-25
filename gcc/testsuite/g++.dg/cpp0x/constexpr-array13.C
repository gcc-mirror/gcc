// PR c++/68087
// { dg-do compile { target c++11 } }

constexpr char c[] = "hello";
constexpr const char *p = c;
constexpr char ch = *(p-1);  // { dg-error "array subscript" }
