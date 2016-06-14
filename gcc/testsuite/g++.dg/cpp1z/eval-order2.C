// P0145R2: Refining Expression Order for C++
// { dg-do run }
// { dg-options "-std=c++1z" }

#include <string>
#define assert(X) if (!(X)) __builtin_abort();

int main()
{
  std::string s = "but I have heard it works even if you don't believe in it" ;
  s.replace(0, 4, "" ).replace( s.find( "even" ), 4, "only" )
    .replace( s.find( " don't" ), 6, "" );

  assert( s == "I have heard it works only if you believe in it" ) ;
}
