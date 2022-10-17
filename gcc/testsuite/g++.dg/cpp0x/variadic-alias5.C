// PR c++/103769
// { dg-do compile { target c++11 } }
// { dg-additional-options "--param=hash-table-verification-limit=1000" }

template <typename T> using t = T;
template <typename...> struct s {};
template <typename...Args> s<t<Args>...> f() { return {};}

int main() { f<void>(); }
