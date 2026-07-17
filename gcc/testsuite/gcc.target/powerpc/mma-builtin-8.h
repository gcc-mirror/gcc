/* Header file for mma-builtin-8.c test - contains test functions only */

void
foo (__vector_pair *dst, __vector_pair *src, long idx)
{
  __vector_pair pair = *src;
  __builtin_vsx_stxvp (pair, 0, dst);
  __builtin_vsx_stxvp (pair, 32, dst);
  __builtin_vsx_stxvp (pair, 64, dst);
  /* Non-constant offset should generate a stxvpx.  */
  __builtin_vsx_stxvp (pair, idx, dst);
  /* Non-aligned offset should generate a pstxvp.  */
  __builtin_vsx_stxvp (pair, 257, dst);
}

#if !__has_builtin (__builtin_vsx_stxvp)
#  error "__has_builtin (__builtin_vsx_stxvp) failed"
#endif
