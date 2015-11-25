// PR c++/58910
// { dg-do compile { target c++11 } }

#include <tuple>

using namespace std;
struct t1{ constexpr t1(){} };
struct t2{ constexpr t2(){} };

int main()
{
  constexpr t1 T1;
  constexpr t2 T2;
  constexpr tuple<t1,t2> Tup1(T1,T2);
  constexpr tuple<t1,t1> Tup2(T1,T1);
  constexpr auto a=get<0>(Tup1 ); //works fine
  constexpr auto b=get<0>(Tup2 ); // error: 
  //'(const std::_Head_base<0ul, t1, true>*)(& Tup2)' 
  //is not a constant expression constexpr auto b=get<0>(Tup2 );
}
