// { dg-do compile { target c++11 } }

#include "pr116847.H"

int a = S<0>::bar ();

int
main ()
{
}
