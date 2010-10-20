// { dg-lto-do link }

#include "20101020-1_0.h"
A::A ()
{
  foo (&A::bar);
}
int main() { return 0; }
