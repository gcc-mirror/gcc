/* Simplified from PR target/5309.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mcpu=ultrasparc" { target sparc64-*-* sparcv9-*-* } } */

#if __INT_MAX__ > 32767
#define PTR_TYPE long
#else
/* For 16-bit ports a long is a 32-bit quantity.  So you cannot
   cast a 32-bit long integer into a pointer which will only be
   16-bits long.  */
#define PTR_TYPE int
#endif

extern PTR_TYPE bar (unsigned int);

PTR_TYPE
foo (PTR_TYPE x, unsigned int y)
{
  return *(((PTR_TYPE *) (bar (y) - 1)) + 1 + (x >> 2) % 359);
}
