#include "static-2.hp"
int bar(void)
{
  static int counter;
  return counter++;
}
