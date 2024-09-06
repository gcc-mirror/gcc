/* { dg-additional-options -std=gnu99 } */

#include <avr/pgmspace.h>

typedef __UINT64_TYPE__ T;

#ifdef __FLASH
T fun64_flash (const __flash T *p)
{
  return *p;
}
#endif

#ifdef __MEMX
T fun64_memx (const __memx T *p)
{
  return *p;
}
#endif

