// PR c++/59636
// { dg-do compile { target c++1y } }

auto f = []() { return []<>() {}; };  // { dg-error "expected identifier" }
