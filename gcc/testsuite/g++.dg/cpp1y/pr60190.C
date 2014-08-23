// PR c++/60190
// { dg-do compile { target c++14 } }

auto f = []<int>() -> int() {}; // { dg-error "returning a function|expected" }
