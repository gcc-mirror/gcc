// { dg-do compile }
// Origin: Graeme Prentice <gprentice at paradise dot net dot nz>
// PR c++/13474: An array domain which is value-dependent must be folded 
//  in time for deduction.

template< int X, int Y, int (*array_ptr)[Y] > 
class A;

int array[5];

template< int X > 
class A<X,5,&array> {};

int main()
{
  A<6,5,&array> z1;  
}
