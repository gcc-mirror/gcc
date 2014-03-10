// { dg-do compile { target c++11 } }
typedef const int* type;

float& foo( const type& ggg );
int& foo( type&& ggg );

void bar( int* someptr )
{
  int& x = foo( someptr );
}
