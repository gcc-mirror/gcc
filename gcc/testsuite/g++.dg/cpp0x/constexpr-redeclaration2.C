// { dg-do compile { target c++11 } }

constexpr float pi = 3.14;
extern const float pi;
constexpr float x = pi;
