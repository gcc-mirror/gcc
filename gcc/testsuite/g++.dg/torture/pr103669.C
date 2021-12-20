// { dg-do run }
/* { dg-additional-options "--param=modref-max-depth=1" } */
#include <list>

typedef std::list<void *> PtrList;

void
SlList (PtrList *l)
{
  PtrList temp = *l;
  PtrList::iterator iter;
  for (iter = temp.begin (); iter != temp.end (); ++iter)
    __builtin_abort ();
}

int
main (void)
{
  PtrList list;
  SlList (&list);
  return 0;
}
