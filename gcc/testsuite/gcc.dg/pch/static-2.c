#include "static-2.h"
int bar(void)
{
  static int counter;
  return counter++;
}
