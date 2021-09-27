/* Test __regio_symbol invalid access diagnostic for HImode.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

extern volatile __regio_symbol
uint16_t __R31; /* { dg-error "only 32-bit access is supported for '__regio_symbol' address space" } */
