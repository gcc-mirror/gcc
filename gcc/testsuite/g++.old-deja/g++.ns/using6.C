// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
#include <vector>

namespace csp {
    using namespace std;
    struct X {
	vector<int> v;
    };
}
