// { dg-do compile { target c++20 } }
// PR c++/117518
template <auto = []<int> (auto) {}> int x;
int y = x<>;
