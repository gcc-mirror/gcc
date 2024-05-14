/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\ml[hb]z\M} } } */

/* Test if by-piece overlap compare is enabled and following case is
   implemented by two overlap word loads and compares.  */

int foo (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 7) == 0;
}
