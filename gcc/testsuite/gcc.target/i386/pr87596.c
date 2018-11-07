/* LRA corner case which triggered a segfault.  */
/* Reduced testcase by Arseny Solokha.  */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O1 -fschedule-insns -ftrapv -funroll-all-loops -fno-tree-dominator-opts -fno-tree-loop-im" } */
    
void
wh (__int128 *ku)
{
  unsigned int *dp;

  while (*ku < 1)
    {
      *dp <<= 32;  /* { dg-warning "left shift count >= width of type" } */
      ++*ku;
    }
}
