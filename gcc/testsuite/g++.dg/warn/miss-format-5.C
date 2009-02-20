/* Test warnings for missing format attributes on function pointers.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wmissing-format-attribute" } */
/* { dg-options "-Wmissing-format-attribute -Wno-abi" { target arm_eabi } } */

#include <stdarg.h>

typedef void (*noattr_t) (const char *, ...);
typedef noattr_t __attribute__ ((__format__(__printf__, 1, 2))) attr_t;

typedef void (*vnoattr_t) (const char *, va_list);
typedef vnoattr_t __attribute__ ((__format__(__printf__, 1, 0))) vattr_t;

noattr_t
foo1 (noattr_t na, attr_t a, int i)
{
  if (i)
    return na;
  else
    return a; /* { dg-warning "candidate" "return type warning" } */
}

attr_t
foo2 (noattr_t na, attr_t a, int i)
{
  if (i)
    return na;
  else
    return a;
}

vnoattr_t
foo3 (vnoattr_t vna, vattr_t va, int i)
{
  if (i)
    return vna;
  else
    return va; /* { dg-warning "candidate" "return type warning" } */
}

vattr_t
foo4 (vnoattr_t vna, vattr_t va, int i)
{
  if (i)
    return vna;
  else
    return va;
}
