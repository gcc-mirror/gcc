/* Test __regio_symbol invalid attempt to get regio variable address.  */

/* { dg-do compile } */
/* { dg-options "-O0" } */

#include "regio.h"

uint32_t test(void)
{
  return *(&__R30+1); /* { dg-error "invalid access to '__regio_symbol' address space" } */
}
