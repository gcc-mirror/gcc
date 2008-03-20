/* Test warnings for missing format attributes on function pointers.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wmissing-format-attribute" } */

#define DONT_GNU_PROTOTYPE
#include "format.h"

typedef void (*noattr_t) (const char *, ...);
typedef noattr_t __attribute__ ((__format__(gnu_attr___printf__, 1, 2))) attr_t;

typedef void (*vnoattr_t) (const char *, va_list);
typedef vnoattr_t __attribute__ ((__format__(gnu_attr___printf__, 1, 0))) vattr_t;

extern void foo1 (noattr_t);
extern void foo2 (attr_t);
extern void foo3 (vnoattr_t);
extern void foo4 (vattr_t);

void
foo (noattr_t na, attr_t a, vnoattr_t vna, vattr_t va)
{
  foo1 (na);
  foo1 (a); /* { dg-warning "candidate" "parameter passing warning" } */
  foo2 (na);
  foo2 (a);

  foo3 (vna);
  foo3 (va); /* { dg-warning "candidate" "parameter passing warning" } */
  foo4 (vna);
  foo4 (va);
}
