// Origin: Mark Mitchell <mark@codesourcery.com>

#include <list>

list<int*> li;

void f ()
{
  li.size ();
}

int main ()
{
  li.push_back (0);
  f ();
}
