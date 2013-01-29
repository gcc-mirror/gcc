/* { dg-do run } */
/* { dg-options "-O3" } */

extern void abort (void);

#include "stdint.h"
#include "vect-ld1r.x"

DEF (int8_t)
DEF (int16_t)
DEF (int32_t)
DEF (int64_t)

#define FOOD(TYPE) \
  foo_ ## TYPE ## _d (&a_ ## TYPE, output_ ## TYPE)

#define FOOQ(TYPE) \
  foo_ ## TYPE ## _q (&a_ ## TYPE, output_ ## TYPE)

#define CHECKD(TYPE) \
  for (i = 0; i < 8 / sizeof (TYPE); i++) \
    if (output_ ## TYPE[i] != a_ ## TYPE) \
      abort ()

#define CHECKQ(TYPE) \
  for (i = 0; i < 32 / sizeof (TYPE); i++) \
    if (output_ ## TYPE[i] != a_ ## TYPE) \
      abort ()

#define DECL(TYPE) \
  TYPE output_ ## TYPE[32]; \
  TYPE a_ ## TYPE = (TYPE)12

int
main (void)
{

  DECL(int8_t);
  DECL(int16_t);
  DECL(int32_t);
  DECL(int64_t);
  int i;

  FOOD (int8_t);
  CHECKD (int8_t);
  FOOQ (int8_t);
  CHECKQ (int8_t);

  FOOD (int16_t);
  CHECKD (int16_t);
  FOOQ (int16_t);
  CHECKQ (int16_t);

  FOOD (int32_t);
  CHECKD (int32_t);
  FOOQ (int32_t);
  CHECKQ (int32_t);

  FOOD (int64_t);
  CHECKD (int64_t);
  FOOQ (int64_t);
  CHECKQ (int64_t);

  return 0;
}
