// { dg-do assemble  }
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
                return vector< vector<s> >(va.size());
        }
};
