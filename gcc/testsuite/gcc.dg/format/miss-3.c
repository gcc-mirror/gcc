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
  noattr_t na1 = na;
  noattr_t na2 = a; /* { dg-warning "candidate" "initialization warning" } */
  attr_t a1 = na;
  attr_t a2 = a;
  
  vnoattr_t vna1 = vna;
  vnoattr_t vna2 = va; /* { dg-warning "candidate" "initialization warning" } */
  vattr_t va1 = vna;
  vattr_t va2 = va;
}
