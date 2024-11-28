/* { dg-do compile } */
/* { dg-options "-O2 -ffinite-loops -Wno-unused-result" } */
/* { dg-additional-options "-fdump-tree-cddce2 -fdump-tree-cddce3" } */
/* { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } } */

#include <string>
#include <vector>
#include <list>
#include <set>
#include <map>

using namespace std;

int foo (vector<string> &v, list<string> &l, set<string> &s, map<int, string> &m)
{
  for (vector<string>::iterator it = v.begin (); it != v.end (); ++it)
    it->length();

  for (list<string>::iterator it = l.begin (); it != l.end (); ++it)
    it->length();

  for (map<int, string>::iterator it = m.begin (); it != m.end (); ++it)
    it->first + it->second.length();

  for (set<string>::iterator it0 = s.begin (); it0 != s.end(); ++it0)
    for (vector<string>::reverse_iterator it1 = v.rbegin(); it1 != v.rend(); ++it1)
      {
        it0->length();
        it1->length();
      }  

  return 0;
}
/* Adding __builtin_unreachable to std::string::size() prevents cddce2 from
   eliminating the loop early, see PR117764.  */
/* { dg-final { scan-tree-dump-not "if" "cddce2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "if" "cddce3"} } */

