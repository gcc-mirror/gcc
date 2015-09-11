// PR c++/59937
// { dg-do compile { target c++11 } }

constexpr const char * const &r = "";
constexpr const char * const &s = r;
