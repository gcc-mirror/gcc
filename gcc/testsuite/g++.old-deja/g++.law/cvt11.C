// Build don't link: 
// GROUPS passed conversions
// cvt file
// Date: Tue, 10 Nov 92 11:08:08 PST
// From: rrh@tera.com (Robert R. Henry)
// Message-Id: <9211101908.AA13557@tera.com>
// Subject: type cast of qualified const member breaks g++2.3.1

#include <stdio.h>

class Thing{
private: int x;
   public: const int N = -1; // ERROR - bad initialization
  Thing(int y);
};

class Bar{ public: void doit(void); };

void Bar::doit(void)
{
  int i, j;
  i = Thing::N;
  printf("i = %d\n", i);

  j = (int)Thing::N;
  printf("i = %d\n", j);
}
Thing::Thing(int y) { x = y; }
int main(){ Bar x; x.doit(); }

