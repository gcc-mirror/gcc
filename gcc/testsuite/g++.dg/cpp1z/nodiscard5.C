// PR c++/80896
// { dg-do compile { target c++11 } }

int x = 42;
[[nodiscard]] int& func() { return x; }

int main() { func(); }  // { dg-warning "ignoring return value" }
