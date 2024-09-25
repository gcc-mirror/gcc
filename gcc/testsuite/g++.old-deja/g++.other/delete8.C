// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for stdlib size_t" { ! hostedlib } }

// Origin: Mark Mitchell <mark@codesourcery.com>

#include <stdlib.h>

struct S {
  ~S ();
};

bool flag;
S* s1;
S* s2;

void* operator new (size_t s)
{
  return malloc (s);
}

void operator delete (void* p)
{
  if (flag && p != s2)
    abort ();
}

S::~S () { 
  if (this != s2)
    abort ();
  s1 = 0;
}

int main () {
  s2 = new S;
  s1 = s2;
  // Turn on the check in `operator delete'.
  flag = true;
  delete s1;
  // Turn it off again so that normal shutdown code works.
  flag = false;
}

