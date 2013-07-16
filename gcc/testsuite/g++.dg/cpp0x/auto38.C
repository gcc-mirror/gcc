// PR c++/57183
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-variable" }

constexpr float PI_0 = 3.1415926F;
constexpr auto PI_1 = 3.1415926F;
const float PI_2 = 3.1415926F;
const auto PI_3 = 3.1415926F;
