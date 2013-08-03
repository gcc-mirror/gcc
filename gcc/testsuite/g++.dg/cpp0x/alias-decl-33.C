// PR c++/51239
// { dg-require-effective-target c++11 }

template<class... x>
class list{};
template<class a, class... b>
using tail=list<b...>;
template <class...T>
void f(tail<T...>);		// { dg-error "alias" }

int main()
{
  f<int,int>({});
}
