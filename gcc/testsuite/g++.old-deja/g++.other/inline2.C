// Origin: Martin Reinecke <martin@MPA-Garching.MPG.DE>
// Build don't link:
// Special g++ Options: -O2 -Winline

#include <cmath>

int main()
{
  double foo = 4.5;
  if (std::abs (0.5-std::abs (foo-0.5)) < 1e-10) foo+=1;
}
