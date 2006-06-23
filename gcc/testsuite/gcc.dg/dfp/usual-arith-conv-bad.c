/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1150 5.4: Usual arithmetic conversions.
   C99 6.3.1.8[1] (New).

   Test arithmetic operators between decimal float types and generic
   float types, which are not allowed.  */

extern _Decimal32 d32a, d32b;
extern _Decimal64 d64a, d64b;
extern _Decimal128 d128a, d128b;
extern float f;
extern double d;
extern long double ld;

extern signed int __attribute__ ((vector_size (16))) vi;

extern _Complex float cf;
extern _Complex double cd;
extern _Complex long double cld;
extern _Complex int ci;

void
foo (void)
{
  /* Mixed operations with decimal and generic float operands.  */
  d32a = d32b + f;	/* { dg-error "" "error.*mix operands of decimal float" } */
  d32a = f * d32b;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d32a *= f;		/* { dg-error "" "error.* mix operands of decimal float" } */
  f += d32b;		/* { dg-error "" "error.* mix operands of decimal float" } */
  d64a = d32a + d;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d64a = d * d128a;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d64a -= d;		/* { dg-error "" "error.* mix operands of decimal float" } */
  d128a = ld * d128b;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d128a = d64b + d;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d128a *= f;		/* { dg-error "" "error.* mix operands of decimal float" } */

  /* Mixed operations with decimal float and a vector type.  */
  d64a = d64b + vi;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d32a *= vi;		/* { dg-error "" "error.* mix operands of decimal float" } */
  d128a = vi - d128b;	/* { dg-error "" "error.* mix operands of decimal float" } */

  /* Mixed operations with decimal float and Complex types.  */
  d32a += ci;		/* { dg-error "" "error.* mix operands of decimal float" } */
  d64a = ci * d32a;	/* { dg-error "" "error.* mix operands of decimal float" } */
  cd = d64a * cd;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d128b = cld * d128b;	/* { dg-error "" "error.* mix operands of decimal float" } */
  d32a = cf * d32b;	/* { dg-error "" "error.* mix operands of decimal float" } */
}
