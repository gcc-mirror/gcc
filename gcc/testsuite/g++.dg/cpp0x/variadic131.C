// PR c++/38543
// { dg-do compile { target c++11 } }

template< typename ... T > void foo( T ... args );
template<> void foo( ){}
template<> void foo(int,double){}
int main()
{
  foo( 0, 0.0 );
  return 55;
}
