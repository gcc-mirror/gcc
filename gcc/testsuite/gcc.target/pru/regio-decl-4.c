/* Test __regio_symbol diagnostics for unsupported access.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

extern volatile uint32_t __regio_symbol *__R30;
uint32_t test_r(void)
{
  return *__R30; /* { dg-error "invalid access to '__regio_symbol' address space" } */
}

void test_w(uint32_t a)
{
  *__R30 = a; /* { dg-error "invalid access to '__regio_symbol' address space" } */
}
