/* Test warnings for missing format attributes on function pointers.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wmissing-format-attribute" } */

#include "format.h"

typedef void (*noattr_t) (const char *, ...);
typedef noattr_t __attribute__ ((__format__(__printf__, 1, 2))) attr_t;

typedef void (*vnoattr_t) (const char *, va_list);
typedef vnoattr_t __attribute__ ((__format__(__printf__, 1, 0))) vattr_t;

void
foo1 (noattr_t na, attr_t a, vnoattr_t vna, vattr_t va)
{
  noattr_t na1, na2;
  attr_t a1, a2;
  
  vnoattr_t vna1, vna2;
  vattr_t va1, va2;

  na1 = na;
  na2 = a; /* { dg-warning "candidate" "assignment warning" } */
  a1 = na;
  a2 = a;
  
  vna1 = vna;
  vna2 = va; /* { dg-warning "candidate" "assignment warning" } */
  va1 = vna;
  va1 = va;
}
