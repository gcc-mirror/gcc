// PR sanitizer/82353
// { dg-do run }
// { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined -std=c++11 -O2 -w" }
// { dg-additional-sources "pr82353-2-aux.cc" }

#include "pr82353-2.h"

unsigned long f, g;
bool h, k, j, i;
unsigned char l, m;
short n;
unsigned o;
F p;

int
main ()
{
  foo ();
  bar ();
}
