// PR c++/105541
// { dg-do compile { target c++20 } }

static_assert(requires { []<typename T>{}; });
