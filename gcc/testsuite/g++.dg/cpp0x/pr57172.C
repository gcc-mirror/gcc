// PR c++/57172
// { dg-do compile { target c++11 } }

template <typename T> int f (T&) { return 0; }
template <typename T> int f (T&&) = delete;
int i;
int j = f (i);
