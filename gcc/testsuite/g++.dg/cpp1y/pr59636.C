// PR c++/59636
// { dg-do compile { target c++14 } }
// { dg-options "" }

auto f = []() { return []<>() {}; };  // { dg-error "expected identifier" }
