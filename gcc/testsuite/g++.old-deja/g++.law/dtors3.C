// { dg-do run  }
// GROUPS passed destructors
// dtor file
// Message-Id: <9301242117.AA04053@cs.rice.edu>
// From: dougm@cs.rice.edu (Doug Moore)
// Subject: 2.3.3: premature dtor of temp?
// Date: Sun, 24 Jan 93 15:17:07 CST

#include <stdio.h>
#include <stdlib.h>

int killed = 0;

class Foo
{
  int a;
public:
  Foo() 
  :a(0) {;}
  ~Foo() { killed++;}
  Foo& operator << (int b)
  {
    a += b;
    if (killed)
      {
	printf ("FAIL\n");
	exit (1);
      }
    return *this;
  }
};

int main()
{
  Foo() << 1 << 3 << 5 << 7;
  printf ("PASS\n");
}

