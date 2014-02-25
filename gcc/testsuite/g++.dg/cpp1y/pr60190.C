// PR c++/60190
// { dg-do compile }
// { dg-options "-std=c++1y" }

auto f = []<int>() -> int() {}; // { dg-error "returning a function|expected" }
