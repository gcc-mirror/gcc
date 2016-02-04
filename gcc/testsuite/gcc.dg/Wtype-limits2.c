/* { dg-do compile } */
/* { dg-options "-Wtype-limits" } */
/* { dg-require-effective-target sync_char_short } */

#include <stdatomic.h>

unsigned foo (unsigned char *x)
{
  if (atomic_load (x) > 1000) /* { dg-warning "comparison is always false due to limited range of data type" } */
    return 0;
  return 1;
}
