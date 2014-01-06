// { dg-do compile }
// { dg-options "-std=c++1y" }

// PR c++/59636

auto f = []() { return []<>() {}; };  // { dg-error "expected identifier" }

