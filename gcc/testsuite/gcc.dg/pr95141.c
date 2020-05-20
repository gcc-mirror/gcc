/* { dg-do compile } */

#include <stdint.h>

uint64_t test(uint8_t IA1)
{
  return (uint8_t)(IA1 & 158) & 1UL; /* { dg-bogus "integer overflow" } */
}
