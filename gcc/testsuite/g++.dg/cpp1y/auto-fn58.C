// PR c++/95820
// { dg-do compile { target c++14 } }

auto (*a)() -> bool = nullptr;
int (*b)() -> bool = nullptr; // { dg-error "function with trailing return type" }
int (**c)() -> bool = nullptr; // { dg-error "function with trailing return type" }
int (&d)() -> bool = nullptr; // { dg-error "function with trailing return type" }
int (*&e)() -> bool = nullptr; // { dg-error "function with trailing return type" }
int (f[])() -> bool = nullptr; // { dg-error "function with trailing return type" }
auto* (*g)() -> bool = nullptr; // { dg-error "function with trailing return type" }
decltype(auto) (*h)() -> bool = nullptr; // { dg-error "invalid use" }
decltype(auto)* (*i)() -> bool = nullptr; // { dg-error "function with trailing return type" }
decltype(auto)& (*j)() -> bool = nullptr; // { dg-error "function with trailing return type" }
