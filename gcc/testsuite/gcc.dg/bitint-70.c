/* PR middle-end/113406 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -fstrub=internal" } */
/* { dg-require-effective-target strub } */

#if __BITINT_MAXWIDTH__ >= 146
_BitInt(146)
#else
_BitInt(16)
#endif
foo (void)
{
  return 0;
}
