/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-lsplit-details" } */
/* { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } } */

#include <string>
#include <map>

using namespace std;

class  A
{
public:
  bool empty;
  void set (string s);
};

class  B
{
  map<int, string> m;
  void f ();
};

extern A *ga;

void B::f ()
{
  for (map<int, string>::iterator iter = m.begin (); iter != m.end (); ++iter)
    {
      if (ga->empty)
        ga->set (iter->second);
    }
}

/* { dg-final { scan-tree-dump-times "loop split on semi-invariant condition at false branch" 1 "lsplit" } } */
