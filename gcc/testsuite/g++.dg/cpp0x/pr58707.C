// PR c++/58707
// { dg-do compile { target c++11 } }

template<int i> class TC { };
constexpr int foo[] = { 42 };
TC<foo[0 > 1]> bar;
