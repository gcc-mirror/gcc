/* { dg-options "-Wint-in-bool-context" } */
/* { dg-do compile } */

#define BITS_PER_UNIT 8

int foo (int count)
{
  int alignment;
 
  alignment = 1;
  while (!(count & alignment)
         && (alignment * 2 * BITS_PER_UNIT)) /* { dg-warning "boolean context" } */
    alignment <<= 1;
  return alignment * BITS_PER_UNIT;
}
