/* { dg-do run } */
/* { dg-options "-O3" } */

extern void abort (void);

#include "stdint.h"
#include "vect-ld1r.x"

DEF (float)
DEF (double)

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
  TYPE a_ ## TYPE = (TYPE)12.2

int
main (void)
{

  DECL(float);
  DECL(double);
  int i;

  FOOD (float);
  CHECKD (float);
  FOOQ (float);
  CHECKQ (float);

  FOOD (double);
  CHECKD (double);
  FOOQ (double);
  CHECKQ (double);

  return 0;
}
