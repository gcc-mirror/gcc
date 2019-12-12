/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned int
yo (unsigned int o0, signed char s1)
{
  for (s1 = 0; s1 < 1; s1 -= 2)
    o0 += o0;

  return o0 + s1;
}
