// { dg-do run  }
// GROUPS passed scoping
// scoping file
// From: svkakkad@cs.utexas.edu (Sheetal V. Kakkad)
// Date:     Tue, 5 Oct 93 12:38:49 -0500
// Subject:  G++ 2.4.5 - global delete operator not called when using "::delete"
// Message-ID: <9310051738.AA14586@boogie.cs.utexas.edu>

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

class foo
{
 public:
  foo () { ; }
  ~foo () { ; }
  void *operator new (size_t);
  void operator delete (void *);
};

void *foo::operator new (size_t size)
{
  return malloc (size);
}

int overloaded_delete = 0;

void foo::operator delete (void *data)
{
  free ((char *) data);
  overloaded_delete++;
}

int main ()
{
  foo *f = new foo;
  foo *ff = ::new foo;
  ::delete ff;		// should call the default delete operator
  delete f;
  if (overloaded_delete == 1)
    printf ("PASS\n");
  else
    { printf ("FAIL\n"); return 1; }
}
