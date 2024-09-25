// { dg-do assemble  }
// { dg-options "-O2 -Winline" }
// { dg-skip-if "requires hosted libstdc++ for cmath" { ! hostedlib } }
// Origin: Martin Reinecke <martin@MPA-Garching.MPG.DE>

#include <cmath>

int main()
{
  double foo = 4.5;
  if (std::abs (0.5-std::abs (foo-0.5)) < 1e-10) foo+=1;
}
