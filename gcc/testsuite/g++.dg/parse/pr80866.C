// { dg-do compile { target c++11 } }
// PR 80866 recycled a lookup too soon.

void pow();
namespace math {
  template <typename T> void pow(T);
}
using namespace math;

decltype(pow<>(0)) z();
