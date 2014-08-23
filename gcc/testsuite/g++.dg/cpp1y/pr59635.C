// PR c++/59635
// { dg-do compile { target c++14 } }

auto f = [] (auto, ...) { return 0; };

int (*p) (int, ...) = f;  // { dg-message "unimplemented" }
