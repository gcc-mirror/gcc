/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PR middle-end/109832 */

/* A conditional compare folds a comparison into the flags.  When the operands
   are plain booleans there is no comparison to fold and the bitwise operation
   is cheaper, so no conditional compare should be formed.  */

_Bool
bool_ior (_Bool x, _Bool y)
{
  return x || y;
}

_Bool
bool_and (_Bool x, _Bool y)
{
  return x && y;
}

extern _Bool t (void);

_Bool
bool_call_ior (void)
{
  _Bool x = t ();
  _Bool y = t ();
  return x | y;
}

_Bool
bool_ior_tree (_Bool a, _Bool b, _Bool c, _Bool d)
{
  return (a | b) | (c | d);
}

/* { dg-final { scan-assembler-not {\tccmp\t} } } */
/* { dg-final { scan-assembler-times {\torr\tw[0-9]+, w[0-9]+, w[0-9]+} 5 } } */
