// PR c++/9452
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

template <int> struct A { static const int i = 1; };
template <int> struct B {};

template <typename> int foo(B<0>)
{
  return 0;
} 

template <typename, int j> B<A<j>::i-1> foo(B<j>)
{
  return B<0>();
} 

int main()
{
  return foo<int>(B<0>());
} 
