/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99" } */
typedef _Complex float GFC_COMPLEX_4;
void product_c4 (GFC_COMPLEX_4 *src, GFC_COMPLEX_4 *dest, int len)
{
  int n;
  GFC_COMPLEX_4 result;
  for (n = 0; n < len; n++, src += 1)
    result *= *src;
  *dest = result;
}
