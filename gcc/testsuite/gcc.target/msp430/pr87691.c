/* PR 87691 - Test that a union containing __int20 and a float is not treated as
   20-bits in size.  */

/* { dg-do compile } */
/* { dg-skip-if "no __int20 for mcpu=msp430" { *-*-* } { "-mcpu=msp430" } { "" } } */
/* { dg-final { scan-assembler-not "MOVX.A" } } */

/* To move a 20-bit value from memory (using indexed or indirect register
   mode), onto the stack (also addressed using indexed or indirect register
   mode), MOVX.A must be used. MOVA does not support these addressing modes.
   Therefore, to check that the union is not manipulated as a 20-bit type,
   test that no MOVX.A instructions are present in the assembly.

   MOVA is used to fill/spill u.i, but if the union is treated as 20 bits in
   size, MOVX.A would be used. No other __int20 operations are present
   in the source, so there will be no valid uses of MOVX.A in the resulting
   assembly.  */

union U1
{
  float f;
  __int20 i;
};

union U2
{
  __int20 i;
  float f;
};

float foo1 (union U1 u)
{
  u.i += 42;
  return u.f;
}

float foo2 (union U2 u)
{
  u.i += 42;
  return u.f;
}
