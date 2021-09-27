/* Test __regio_symbol diagnostics for unsupported declarations.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

extern volatile __regio_symbol
uint32_t __R30[10]; /* { dg-error "aggregate types are prohibited in '__regio_symbol' address space" } */

/* { dg-warning "'__R31' initialized and declared 'extern'" "" { target *-*-* } 0 } */
extern volatile __regio_symbol
uint32_t __R31 = 2; /* { dg-error "variables in '__regio_symbol' address space cannot have initial value" } */
