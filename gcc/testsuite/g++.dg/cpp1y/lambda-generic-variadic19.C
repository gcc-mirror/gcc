// PR c++/86728
// { dg-do compile { target c++14 } }

auto c = [](auto x ...) { };
