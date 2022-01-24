/* Test __regio_symbol invalid access diagnostic for QImode.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

extern volatile __regio_symbol
uint8_t __R31; /* { dg-error "only 32-bit access is supported for '__regio_symbol' address space" } */
