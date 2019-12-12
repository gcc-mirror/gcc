/* PR tree-optimization/87490 - ICE in expand_builtin_strnlen with a constant
   argument and non-constant bound
   { dg-do compile }
   { dg-options "-O1 -Wall -fno-optimize-strlen" }  */

void test_O1 (int i)
{
  int n = (i & 3) | 1;

  /* The ICE here triggers at -O1, with tree-ssa-strlen disabled.  */
  if (__builtin_strnlen ("", n) != 0)
    __builtin_abort ();
}
