/* Test __regio_symbol diagnostics for unsupported declarations.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

volatile __regio_symbol
uint32_t __R30; /* { dg-error "variables in '__regio_symbol' address space must be declared 'extern'" } */

extern __regio_symbol
uint32_t __R31; /* { dg-error "variables in '__regio_symbol' address space must be declared 'volatile'" } */

extern volatile
__regio_symbol uint32_t __R32; /* { dg-error "register name '__R32' not recognized in '__regio_symbol' address space" } */
