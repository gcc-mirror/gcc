// PR c++/59636
// { dg-do compile }
// { dg-options "-std=c++1y" }

auto f = []() { return []<>() {}; };  // { dg-error "expected identifier" }
