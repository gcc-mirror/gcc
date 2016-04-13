// PR c++/66786
// { dg-do compile { target c++14 } }

template <typename... T> auto list = [](T... xs) { [=](auto f) { f(xs...); }; };
int main() { list<int>(0); }
