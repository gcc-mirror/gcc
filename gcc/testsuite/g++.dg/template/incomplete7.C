// PR c++/72849

extern struct Foo a;
template <typename> void fn1() { a; }
