/* N1150 4: Characteristics of decimal floating types (not explicit)
   C99 5.2.4.2.2: Characteristics of floating types.
   A few simple checks on arithmetic operations.  Based on nan-1.c with
   the consideration of negative zero.  */

#include "dfp-dbg.h"

int main()
{
  _Decimal32 d32;
  _Decimal64 d64;
  _Decimal128 d128;

  /* Verify that division by negative zero produces a negative infinity 
     result.  */
  d32 = 123.45f;
  if (d32/-0.0df != -__builtin_infd32())
    FAILURE
  if (123.45df/-0.0df != -__builtin_infd32())
    FAILURE
  
  d64 = 123.45f;
  if (d64/-0.0dd != -__builtin_infd64())
    FAILURE
  if (123.45dd/-0.0dd !=  -__builtin_infd64())
    FAILURE

  d128 = 123.45f;
  if (d128/-0.0dl != -__builtin_infd64())
    FAILURE
  if (123.45dl/-0.0dl != -__builtin_infd128())
    FAILURE

  d32 = 0.0df;
  if (!__builtin_isnand32(-(d32/-0.0df)))
    FAILURE
  if (!__builtin_isnand32(-(0.0df/-0.0df)))
    FAILURE

  d64 = 0.0dd;
  if (!__builtin_isnand64(-(d64/-0.0dd)))
    FAILURE
  if (!__builtin_isnand64(-(0.0dd/-0.0dd)))
    FAILURE

  d128 = 0.0dl;
  if (!__builtin_isnand128(-(d128/-0.0dl)))
    FAILURE
  if (!__builtin_isnand128(-(0.0dl/-0.0dl)))
    FAILURE

  FINISH
}
