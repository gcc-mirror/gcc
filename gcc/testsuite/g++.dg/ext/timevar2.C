// PR c++/57524
// { dg-options "-ftime-report" }
// { dg-prune-output "wall" }
// { dg-prune-output "times" }
// { dg-prune-output "TOTAL" }
// { dg-prune-output "checks" }

namespace detail {
namespace indirect_traits {}
using namespace indirect_traits;
void fn1() {
using namespace detail;
}
}
