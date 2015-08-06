// PR c++/66533
// { dg-do compile { target c++14 } }
auto a([](auto) -> decltype((void)0) {});
