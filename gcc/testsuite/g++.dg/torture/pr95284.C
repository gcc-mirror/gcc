// { dg-do compile }
// { dg-require-effective-target lp64 }

#include <algorithm>

short a;
unsigned long long c;
char d;
unsigned e;

void f()
{
  for (;;)
    for (char b = 0; b < 19; b += 2)
      a = std::min((1 ? d : 0) ? e : c, (unsigned long long)72252803048);
}
