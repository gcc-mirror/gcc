/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

/* PR tree-optimization/21029

   f() used to get optimized to an infinite loop by tree-vrp, because
   j is assumed to be non-negative.  Even though the conversion from
   unsigned to signed has unspecified results if the expression value
   is not representable in the signed type, the compiler itself (e.g.,
   the Ada front end) depends on wrap-around behavior.  */

unsigned int f(void) {
  unsigned char i = 123;
  signed char j;

  do
    if ((j = (signed char) i) < 0)
      break;
    else
      i++;
  while (1);

  return i;
}

/* Now let's torture it a bit further.  Narrowing conversions need
   similar treatment.  */

unsigned int f1 (void) {
  unsigned short i = 123;
  signed char j;

  do
    if ((j = (signed char) i) < 0)
      break;
    else
      i++;
  while (1);

  return i;
}

/* And so do widening conversions.  */

unsigned int f2 (void) {
  unsigned char i = 123;
  signed short j;

  do
    if ((j = (signed short) (signed char) i) < 0)
      break;
    else
      i++;
  while (1);

  return i;
}

/* Check same-sign truncations with an increment that turns into
   decrements.  */

unsigned int f3 (void) {
  signed short i = 5;
  signed char j;

  do
    if ((j = (signed char) i) < 0)
      break;
    else
      i += 255;
  while (1);

  return i;
}

/* Check that the truncation above doesn't confuse the result of the
   test after a widening conversion.  */

unsigned int f4 (void) {
  signed short i = -123;
  signed int j;

  do
    if ((j = (signed int) (signed char) i) > 0)
      break;
    else
      i += 255;
  while (1);

  return i;
}

/* Even if we omit the widening truncation, the narrowing truncation
   is implementation-defined.  */

unsigned int f5 (void) {
  signed long i = -123;
  signed char j;

  do
    if ((j = (signed char) i) > 0)
      break;
    else
      i += 255;
  while (1);

  return i;
}

int main (void) {
  f ();
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  return 0;
}
