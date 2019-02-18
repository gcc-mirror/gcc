// { dg-do assemble  }
#include <stdio.h>

template <int n1>
double val <int> () // { dg-error "expected" "" { target c++17_down } } bogus code
// { dg-error "template-id .val<int>. in declaration of primary template" "" { target c++2a } .-1 }
{                          
   return (double) n1;
}

int main ()
{
   printf ("%d\n", val<(int)3> ()); // { dg-error "" "" { target c++17_down } } val undeclared
}
