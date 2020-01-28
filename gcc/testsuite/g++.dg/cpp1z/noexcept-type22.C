// PR c++/90731
// { dg-do compile { target c++17 } }

typedef void T() noexcept(true);
T t;
void t() noexcept(true);
