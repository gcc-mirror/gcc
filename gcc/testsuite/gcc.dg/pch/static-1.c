#include "static-1.h"
static int bar(void)
{
  static int counter;
  return counter++;
}
