#include <stdio.h>

template <int n1>
double val <int> ()
{                          // ERROR - bogus code
   return (double) n1;
};

int main ()
{
   printf ("%d\n", val<(int)3> ());
}
