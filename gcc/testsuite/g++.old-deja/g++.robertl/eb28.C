//Build don't link: 
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
