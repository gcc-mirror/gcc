// PR c++/51239
// { dg-require-effective-target c++11 }
// This variant should work because tail is equivalent to list.

template<class y, class... x>
class list{};
template<class a, class... b>
using tail=list<a, b...>;
template <class...T>
void f(tail<T...>);

int main()
{
  f<int,int>({});
}
