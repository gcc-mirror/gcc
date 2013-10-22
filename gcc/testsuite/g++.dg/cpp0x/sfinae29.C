// PR c++/51047
// { dg-options -std=c++11 }

template<typename T> T &&declval();
template<class T> decltype(declval<T>().x) f(T *);
template<class T> char f(T);
struct B1{ int x; };
struct B2{ int x; };
struct D : public B1, B2{};
struct S { int x; };
int main()
{
  S *p = nullptr;
  static_assert(sizeof(f(p)) == sizeof(int), "");
  D *q = nullptr;
  static_assert(sizeof(f(q)) == 1u, "");
}
