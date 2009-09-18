/* N1150 4: Characteristics of decimal floating types (not explicit)
   C99 5.2.4.2.2: Characteristics of floating types.
   A few simple checks on arithmetic operations. */

#include "dfp-dbg.h"

int main()
{
  /* Assumes rounding mode. */
  if (9999999.E90DF + 1.E90df != __builtin_infd32 ())
    FAILURE

  if (!__builtin_isinfd32 (9999999.E90DF + 1.E90df))
    FAILURE

  if (9.999999999999999E384dd + .000000000000001e384dd 
      != __builtin_infd32 ())
    FAILURE

  if (-9999999.E90DF - 1.E90df != -__builtin_infd32 ())
    FAILURE

  if (!__builtin_isinfd32 (9.999999999999999E384dd + .000000000000001e384dd))
    FAILURE

  if (7.999999999999999999999999999999999E6144dl + 3.0E6144dl
      != __builtin_infd32 ())
    FAILURE

  if (__builtin_infd32 () * __builtin_infd32 () != __builtin_infd32 ())
    FAILURE

  if (__builtin_infd32 () * 2 != __builtin_infd32 ())
    FAILURE

  if (__builtin_infd64 () * -5 != -__builtin_infd32 ())
    FAILURE

  if (!__builtin_isinfd128 (__builtin_infd32 () / 4))
    FAILURE

  if (__builtin_infd64 () != __builtin_infd128 ())
    FAILURE

  if (!__builtin_isinfd64 (__builtin_infd128 ()))
    FAILURE

  if (__builtin_finited64 (__builtin_infd32 () * 4))
    FAILURE

  if (!__builtin_finited128 (9.999999E90DL - 1.E90dd))
    FAILURE

  if (__builtin_finited128 (__builtin_infd32 () *__builtin_infd128 ()))
    FAILURE

  if (__builtin_finited32 (__builtin_nand32 ("")))
    FAILURE

  FINISH
}
