// PR c++/90548
// { dg-do compile { target c++14 } }

struct S { S (void ()); };
S foo([] (auto...) { });
S foo2{[] (auto...) {}};
S foo3 = {[] (auto...) {}};

struct W { W(void (int)); };
W bar([](auto...) { });
W bar2{[](auto...) { }};
W bar3 = {[](auto...) { }};

struct T { T(void (int, int)); };
T qux([](auto...) { });
T qux2{[](auto...) { }};
T qux3 = {[](auto...) { }};

struct R { R(void (int, int, int, int, int, int, int, int, int, int)); };
R baz([](auto...) { });
R baz2{[](auto...) { }};
R baz3 = {[](auto...) { }};
