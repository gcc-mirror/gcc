/* PR middle-end/68001 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

#include <vector>

std::vector<double> f() {
  std::vector<double> v;
  return v;
}

int main()
{
  std::vector<double> x = _Cilk_spawn f ();
  std::vector<double> y = f();
  _Cilk_sync;
  return 0;
}
