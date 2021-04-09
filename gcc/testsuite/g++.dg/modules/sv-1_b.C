// { dg-additional-options {-std=c++2a -fmodules-ts -fno-module-lazy} }
#include "sv-1.h"
import Hello;

#if 0
int main ()
{
  SayHello ("World");
}
#endif

void foo ()
{
  is_same<long, int> q;
}
