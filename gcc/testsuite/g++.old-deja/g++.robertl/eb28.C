// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
#include <vector>

using namespace std;

enum s { S };
class a
{
        vector<s> vs;
        friend class b;
};
struct b
{
        vector<a> va;
        operator vector< vector<s> >()
        {
                vector< vector<s> > vvs(va.size());
                return vvs;
        }
};
