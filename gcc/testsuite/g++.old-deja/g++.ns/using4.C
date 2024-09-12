// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
//Build don't link
#include <vector>
namespace csp {
using namespace std::vector;  // { dg-error "" } vector is not a namespace
}
