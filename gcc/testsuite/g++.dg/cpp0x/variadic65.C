// { dg-do compile { target c++11 } }
struct unused;
template<typename T1 = unused, typename T2 = unused, typename T3 = unused,
         typename T4 = unused, typename T5 = unused, typename T6 = unused>
struct tuple {};

template<typename... Args>
tuple<Args...> foo() { } // { dg-bogus "cannot expand" "" }

int main()
{
  foo<int,int,int,int,int,int>();
}
