// PR c++/59635
// { dg-do compile }
// { dg-options "-std=c++1y" }

auto f = [] (auto, ...) { return 0; };

int (*p) (int, ...) = f;  // { dg-message "unimplemented" }
