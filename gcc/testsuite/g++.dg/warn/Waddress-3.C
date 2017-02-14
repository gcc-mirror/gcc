// PR c++/65168
// { dg-do compile { target c++11 } }
// { dg-options -Waddress }
// We shouldn't warn in unevaluated context about the address of a reference
// always being true.

template <class T, class U>
auto f(U&& u) -> decltype(T(u)) { }

int main()
{
  bool ar[4];
  f<bool>(ar);
}
