// { dg-do assemble  }
#include <stdio.h>

template <int n1>
double val <int> () // { dg-error "" } bogus code
{                          
   return (double) n1;
}

int main ()
{
   printf ("%d\n", val<(int)3> ()); // { dg-error "" } val undeclared
}
