// PR c++/51507
// { dg-options -std=c++0x }

template<typename ...>
struct foo { typedef void type; };
template<typename ...Ts>
auto g(Ts ...ts)->
  typename foo<decltype(ts)...>::type
{}
int main() {
  g(42);
}
