/* { dg-additional-options "-O1" } */

#include "tree-vect.h"

unsigned char b;
unsigned short c;

[[gnu::noipa]]
void f() {
  unsigned char bb = b;
  unsigned short cc = c;
  do {
    bb--;
    cc--;
    c--;
  } while (bb);
  //c = cc;
  b = bb;
}

int main()
{
  check_vect ();
  f();
  if (c != 65280)
    __builtin_abort();
  return 0;
}
