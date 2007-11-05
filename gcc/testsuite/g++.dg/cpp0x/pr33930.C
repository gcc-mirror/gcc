// { dg-options "-std=c++0x" }
typedef const int* type;

float& foo( const type& ggg );
int& foo( type&& ggg );

void bar( int* someptr )
{
  int& x = foo( someptr );
}
