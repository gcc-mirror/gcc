// GROUPS passed arrays
// array file
// Message-Id: <9204120353.AA06266@cs.rice.edu>
// From: dougm@cs.rice.edu (Doug Moore)
// Subject: constructors not called on new'ed array elements
// Date: Sat, 11 Apr 92 22:53:35 CDT

#include <stdio.h>

int i = 0;

class foo
{
private:
  static foo *array;
public:
  foo()
    {
      i++;
    }
};

foo* foo::array = new foo [5];

int main()
{
  if (i != 5)
    { printf ("FAIL\n"); return 1; }
  else
    printf ("PASS\n");
}
