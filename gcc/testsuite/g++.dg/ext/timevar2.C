// PR c++/57524
// { dg-options "-ftime-report" }
// { dg-prune-output "Time variable" }
// { dg-prune-output "k" }
// { dg-prune-output " 0 " }
// { dg-prune-output "checks" }
// { dg-prune-output "\[0-9\]+%" }

namespace detail {
namespace indirect_traits {}
using namespace indirect_traits;
void fn1() {
using namespace detail;
}
}
