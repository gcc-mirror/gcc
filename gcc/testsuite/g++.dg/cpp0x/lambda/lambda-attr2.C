// PR c++/60503
// { dg-do compile { target c++11 } }

auto l = []() mutable noexcept [[ ]] {};
auto m = []() [[ ]] mutable noexcept {}; // { dg-error "" }
