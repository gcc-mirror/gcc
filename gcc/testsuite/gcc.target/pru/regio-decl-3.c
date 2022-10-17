/* Test __regio_symbol diagnostics for unsupported declarations.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

uint32_t __regio_symbol *test1(void); /* { dg-error "pointers to '__regio_symbol' address space are prohibited" } */

void test2(uint32_t __regio_symbol __R30); /* { dg-error "'__regio_symbol' specified for parameter '__R30'" } */

void test3(uint32_t __regio_symbol *__R30); /* { dg-error "pointers to '__regio_symbol' address space are prohibited" } */

typedef volatile uint32_t __regio_symbol * regio_type1_t; /* { dg-error "pointers to '__regio_symbol' address space are prohibited" } */

struct A {
  uint32_t __regio_symbol *__R30; /* { dg-error "pointers to '__regio_symbol' address space are prohibited" } */
  uint32_t __regio_symbol __R31; /* { dg-error "__regio_symbol' specified for structure field '__R31'" } */
};
