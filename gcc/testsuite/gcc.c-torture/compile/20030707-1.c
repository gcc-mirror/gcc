/* PR c/11449.  */

/* sign_bit_p() in fold-const.c failed to notice that (int) 0x80000000
   was the sign bit of m.  As a result, fold_single_bit_test()
   returned ((unsigned int) m >> 31), and that was eventually passed
   to invert_truthvalue(), which did not know how to handle
   RROTATE_EXPR, causing an ICE.  */

int
foo (int m)
{
  return !(m & ((int) 0x80000000));
}
