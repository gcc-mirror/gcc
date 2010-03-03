// { dg-options "-fabi-version=0" }

#include "20100302.h"

int main()
{
  f(& A<mm128>::t);
}
