/* PR ipa/81520 */
/* { dg-do compile } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-effective-target fpic } */

char
a (int b)
{
  a (b);
  return 0;
}
