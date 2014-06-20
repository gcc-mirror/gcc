/* { dg-lto-do link } */
/* { dg-lto-options { { -fno-short-enums -Wl,-Ur,--no-enum-size-warning -Os -nostdlib -flto } } } */

#include <stdlib.h>

enum enum_size_attribute
{
  small_size, int_size
};

struct debug_ABI_enum_size
{
  enum enum_size_attribute es;
};

int
foo1 (struct debug_ABI_enum_size *x)
{
  return sizeof (x->es);
}

/* { dg-final { object-readelf Tag_ABI_enum_size int { target arm_eabi } } } */
