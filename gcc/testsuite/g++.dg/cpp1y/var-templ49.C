// PR c++/66786
// { dg-do compile { target c++14 } }

int f (int, bool);

template <typename>
auto list = [](auto... xs) { return [=](auto f, auto... ys) { return f(xs..., ys...); }; };

const int &a = list<int>(0)(f, true);
