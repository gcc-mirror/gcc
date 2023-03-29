/* Test the LDAR instruction generation from atomic acquire loads.  */
/* { dg-do assemble } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+norcpc"

uint8_t v_uint8_t;
uint16_t v_uint16_t;
uint32_t v_uint32_t;
uint64_t v_uint64_t;

/*
** load_uint8_t:
**      ...
**      ldarb	w0, \[x[0-9]+\]
**      ret
*/

uint8_t
load_uint8_t (void)
{
  return __atomic_load_n (&v_uint8_t, __ATOMIC_ACQUIRE);
}

/*
** load_uint16_t:
**      ...
**      ldarh	w0, \[x[0-9]+\]
**      ret
*/

uint16_t
load_uint16_t (void)
{
  return __atomic_load_n (&v_uint16_t, __ATOMIC_ACQUIRE);
}

/*
** load_uint32_t:
**      ...
**      ldar	w0, \[x[0-9]+\]
**      ret
*/

uint32_t
load_uint32_t (void)
{
  return __atomic_load_n (&v_uint32_t, __ATOMIC_ACQUIRE);
}

/*
** load_uint64_t:
**      ...
**      ldar	x0, \[x[0-9]+\]
**      ret
*/

uint64_t
load_uint64_t (void)
{
  return __atomic_load_n (&v_uint64_t, __ATOMIC_ACQUIRE);
}

