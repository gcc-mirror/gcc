// Build don't link:

#include <cstddef>

void f()
{
  int i;
  float f;

  i != NULL; // WARNING - NULL used in arithmetic
  f != NULL; // WARNING - NULL used in arithmetic
}
