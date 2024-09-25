// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for list" { ! hostedlib } }
// Gives ICE on EGCS release branch as of 98/06/08 on i686-pc-linux-gnulibc1)
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de>    
// Message-Id: <199806081358.PAA02505@achibm5.chemie.uni-karlsruhe.de> 
#include <list>
#include <functional>
#include <algorithm>
#include <cassert>
using namespace std;
int main()
{
      list<int> l;
      l.push_back(1);
      l.push_back(2);

      list<int>::iterator it =
              find_if( l.begin(), l.end(),
                       // This is a typo, it should be bind2nd, but an
                       // ICE is not a very helpful diagnostic!
                       binder2nd( equal_to<int>(), 2 ) ); // { dg-error "" "" { target c++14_down } }
      assert( *(it) == 2 );
}

