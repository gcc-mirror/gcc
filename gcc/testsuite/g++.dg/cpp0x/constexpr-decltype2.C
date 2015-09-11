// PR c++/65942
// { dg-do compile { target c++11 } }

template <typename T> constexpr int f(T t) { return t; }
template <typename T, typename = decltype(f(T()))> void g(T) { }
void g(...) { }
int main() { g(""); }
