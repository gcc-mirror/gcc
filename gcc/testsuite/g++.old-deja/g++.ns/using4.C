// { dg-do assemble  }
//Build don't link
#include <vector>
namespace csp {
using namespace std::vector;  // { dg-error "" } vector is not a namespace
}
