// PR c++/99478
// { dg-do compile { target c++20 } }

template <decltype ([] {})> auto f() {}

int main() { f<{}>(); }
