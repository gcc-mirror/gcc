// PR c++/56060
// { dg-do compile { target c++11 } }

template<typename T> struct baz { };
template<typename T> T bar();

template<typename T, typename ... U>
baz<decltype(bar<T>()(bar<U> ...))>  // { dg-error "cannot be used" }
foo();

int main()
{
  foo<int>();     // { dg-error "no matching" }
  return 0;
}
