#include "static-1.hp"
static int bar(void)
{
  static int counter;
  return counter++;
}
