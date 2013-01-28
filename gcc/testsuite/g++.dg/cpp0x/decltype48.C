// PR c++/56059
// { dg-options -std=c++11 }

typedef int Int;
template<typename T> struct baz { };
template<typename T> T bar();

template<typename T, typename ... U>
baz<decltype(bar<Int>(bar<U>() ...))> // { dg-error "no match" }
foo();

int main()
{
  foo<int, int>();		// { dg-error "no match" }
  return 0;
}
