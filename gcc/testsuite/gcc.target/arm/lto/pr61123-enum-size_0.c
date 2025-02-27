/* { dg-lto-do link } */
/* { dg-lto-options { { -fno-short-enums -Os -flto } } } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel -Wl,-Ur,--no-enum-size-warning -nostdlib" } */

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
