/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Ensure that we error out in case no hard regs are available for an operand
   with constraint y.  The position/order of the y-constrained operand does not
   matter.  */

void
test (void)
{
  int x, a, b, c, d, e, f, g, h;

  __asm__ __volatile__ ("" :
      "={v0}" (a),
      "={v1}" (b),
      "={v2}" (c),
      "={v3}" (d),
      "={v4}" (e),
      "={v5}" (f),
      "={v6}" (g),
      "={v7}" (h));

  __asm__ __volatile__ ("" : /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
      "=y" (x),
      "={v0}" (a),
      "={v1}" (b),
      "={v2}" (c),
      "={v3}" (d),
      "={v4}" (e),
      "={v5}" (f),
      "={v6}" (g),
      "={v7}" (h));

  __asm__ __volatile__ ("" : /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
      "={v0}" (a),
      "={v1}" (b),
      "={v2}" (c),
      "={v3}" (d),
      "=y" (x),
      "={v4}" (e),
      "={v5}" (f),
      "={v6}" (g),
      "={v7}" (h));

  __asm__ __volatile__ ("" : /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
      "={v0}" (a),
      "={v1}" (b),
      "={v2}" (c),
      "={v3}" (d),
      "={v4}" (e),
      "={v5}" (f),
      "={v6}" (g),
      "={v7}" (h),
      "=y" (x));
}
