/* { dg-do assemble } */
/* { dg-options "-O2 -march=armv8-a+nocrc -save-temps" } */

#include "arm_acle.h"

/* Check that enabling an ISA feature using an attribute in a file
   compiled without that attribute works.  */

__attribute__ ((target ("+crc")))
uint32_t
foo (uint32_t a, uint8_t b)
{
  return __crc32b (a, b);
}

__attribute__ ((target ("arch=armv8-a+crc")))
uint32_t
fooarch (uint32_t a, uint8_t b)
{
  return __crc32b (a, b);
}

__attribute__ ((target ("cpu=cortex-a53+crc")))
uint32_t
foocpu (uint32_t a, uint8_t b)
{
  return __crc32b (a, b);
}

/* { dg-final { scan-assembler-times "crc32b\tw..?, w..?, w..?\n" 3 } } */
