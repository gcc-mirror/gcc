/* Test support of scalar_storage_order attribute */

/* { dg-do compile } */

#include <stdarg.h>

int x;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
struct __attribute__((scalar_storage_order("big-endian"))) Rec
{
  va_list v;
};
#else
struct __attribute__((scalar_storage_order("little-endian"))) Rec
{
  va_list v;
};
#endif

void foo (int i, ...)
{
  struct Rec a;
  va_start (a.v, i);
  a.v = 0, x = va_arg (a.v, int); /* { dg-error "type|reverse storage order" } */
  va_end (a.v);
}
