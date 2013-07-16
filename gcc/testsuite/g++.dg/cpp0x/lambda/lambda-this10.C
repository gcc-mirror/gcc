// PR c++/54383
// { dg-do compile { target c++11 } }

auto foo = [&](int a) { return a > this->b; }; // { dg-error "this" }
