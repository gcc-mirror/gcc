#include "20090312.h"

extern "C" {
    extern enum Values x;
    extern JSErrorCallback p;
};

main()
{
  if ( x == ONE && p == 0)
    return 0;

  return 1;
}
