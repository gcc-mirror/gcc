// GROUPS passed constructors
// ctor file
// Message-Id: <199212160609.AA18247@phecda.cs.sfu.ca>
// From: Taj Khattra <khattra@cs.sfu.ca>
// Subject: gcc 2.3.1 global ctor bug ?
// Date: Tue, 15 Dec 92 22:09:37 PST

#include <stdio.h>

struct foo {
     foo() : index(-1) {}
     int index;
};

foo *arr = new foo[2];

int main()
{
  if (arr[0].index == -1
      && arr[1].index == -1)
    printf ("PASS\n");
  else
    { printf ("FAIL\n"); return 1; }
}
